module Validator (payToPassword, spendFromPassword) where

import Prelude
  ( bind
  , discard
  , Unit
  , (<<<)
  , ($)
  , (<$>)
  , (*)
  , (<>)
  , show
  , pure
  , mempty
  )
import Data.Maybe (Maybe(Nothing))
import Data.Function.Uncurried (Fn3, mkFn3)
import Data.Array (head)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Contract.Monad (Contract)
import Contract.PlutusData
  ( toData
  , PlutusData
  , Redeemer(..)
  , unitDatum
  )
import Contract.Transaction
  ( TransactionHash
  , _input
  , submitTxFromConstraints
  , lookupTxHash
  , awaitTxConfirmed
  )
import Contract.Log (logInfo')
import Contract.Scripts (validatorHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints
  ( mustPayToScript
  , mustSpendScriptOutput
  , DatumPresence(DatumWitness)
  ) as Constraints
import Contract.Address (scriptHashAddress)
import Contract.Utxos (utxosAt)
import Data.Lens (view)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Contract.ScriptLookups (ScriptLookups, validator, unspentOutputs) as Lookups
import Contract.Value (lovelaceValueOf) as Value
import Control.Promise (Promise)
import Contract.Config (ContractParams)
import Utils
import PlyScripts (passwordValidator)

{-
These are wrappers around the `payToPassword'` and `spendFromPassword'` endpoints which allow
them to be called like ordinary JS functions - e.g. `payToPassword(arg1,arg2,arg3)`

NOTE: If you do not use the mkFnX wrappers, you will likely get an error, but it appears that
      un-wrapped functions can still be called in `payToPassword(arg1)(arg2)(arg3)` style.
-}
payToPassword :: Fn3 ContractParams ByteArray BigInt (Promise TransactionHash)
payToPassword = mkFn3 $ \cfg pw adaVal -> execContract' cfg (payToPassword' pw adaVal)

spendFromPassword :: Fn3 ContractParams TransactionHash ByteArray Unit
spendFromPassword = mkFn3 $ \cfg txhash pw ->
  execContract cfg $ spendFromPassword' pw txhash

{-
Password Validator endpoints. These were adapted from the alwaysSucceeds examples in
the CTL `examples` directory, which contains many other example endpoints and can be viewed here:
https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/examples
-}
payToPassword'
  :: ByteArray
  -> BigInt
  -> Contract TransactionHash -- txhash : Uint8Array
payToPassword' pw adaVal = do
  logInfo' "Paying to Password validator..."
  validator <- passwordValidator pw
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

-- Spend from password endpoint
spendFromPassword'
  :: ByteArray
  -> TransactionHash
  -> Contract Unit
spendFromPassword' pw txId = do
  -- pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii pwStr
  validator <- passwordValidator pw
  let
    vhash = validatorHash validator

    scriptAddress =
      scriptHashAddress vhash Nothing

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
    lookups = Lookups.validator validator
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints =
      Constraints.mustSpendScriptOutput txInput (Redeemer <<< toData $ pw)

  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."
