module Validator (payToPassword, spendFromPassword) where

import Utils
import Contract.Address (scriptHashAddress)
import Contract.Config (ContractParams)
import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.PlutusData (toData, PlutusData, Redeemer(..), unitDatum)
import Contract.ScriptLookups (ScriptLookups, validator, unspentOutputs) as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionHash, _input, submitTxFromConstraints, lookupTxHash, awaitTxConfirmed)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustPayToScript, mustSpendScriptOutput, DatumPresence(DatumWitness)) as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (lovelaceValueOf) as Value
import Control.Promise (Promise)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Either (either)
import Data.Function.Uncurried (Fn3, mkFn3)
import Data.Lens (view)
import Data.Maybe (Maybe(Nothing))
import PlyScripts (makePasswordValidator)
import Prelude (bind, discard, Unit, (<<<), ($), (<$>), (*), (<>), show, pure, mempty)

{-
These are wrappers around the `payToPassword'` and `spendFromPassword'` endpoints which allow
them to be called like ordinary JS functions - e.g. `payToPassword(arg1,arg2,arg3)`

NOTE: If you do not use the mkFnX wrappers, 
the un-wrapped functions can still be called in `payToPassword(arg1)(arg2)(arg3)` style.
-}
payToPassword :: Fn3 ContractParams ByteArray BigInt (Promise TransactionHash)
payToPassword = mkFn3 $ \cfg pw adaVal -> execContract' cfg (payToPassword' pw adaVal)

spendFromPassword :: Fn3 ContractParams TransactionHash ByteArray Unit
spendFromPassword =
  mkFn3
    $ \cfg txhash pw ->
        execContract cfg $ spendFromPassword' pw txhash

{-
Password Validator endpoints. These were adapted from the alwaysSucceeds examples in
the CTL `examples` directory, which contains many other example endpoints and can be viewed here:
https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/examples

payToPassword' is a contract submitting and awaiting transaction that sends funds to 
the password validator script, for the chosen password.
-}
payToPassword' ::
  ByteArray -- password ->
    BigInt -- ada amount ->
    Contract
    TransactionHash -- txhash : Uint8Array
payToPassword' pw adaVal = do
  logInfo' "Paying to Password validator..."
  validator <- either throwContractError pure $ makePasswordValidator pw
  -- adaVal <- liftErr "Error: Invalid ada value string" $  BigInt.fromString adaValStr
  let
    vhash = validatorHash validator

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScript vhash unitDatum
        Constraints.DatumWitness
        $ Value.lovelaceValueOf
        $ (adaVal * BigInt.fromInt 1_000_000)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty
  logInfo' "Attempting to submit..."
  txhash <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txhash
  logInfo' "Successfully paid to password validator"
  pure txhash

{- Spend from password endpoint,
a contract that submits and awaits a transaction that attempts to spend some UTXO owned by password validator,
by providing the password in the redeemer. 

Application tracks what was the last submitted paying to password transaction,
the second argument is its hash.
-}
spendFromPassword' ::
  ByteArray -- password ->
    TransactionHash -- hash of last locking tx ->
    Contract
    Unit
spendFromPassword' pw txId = do
  validator <- either throwContractError pure $ makePasswordValidator pw
  let
    vhash = validatorHash validator

    scriptAddress = scriptHashAddress vhash Nothing
  utxos <- utxosAt scriptAddress
  txInput <-
    liftErr
      ( "The id "
          <> show txId
          <> " does not have output locked at: "
          <> show scriptAddress
      )
      (view _input <$> head (lookupTxHash txId utxos))
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups =
      Lookups.validator validator
        <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustSpendScriptOutput txInput (Redeemer <<< toData $ pw)
  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
