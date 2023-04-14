module Validator (payToPassword, spendFromPassword) where

import Prelude
  ( class Eq
  , class Ord
  , Void
  , bind
  , discard
  , join
  , Unit
  , (<<<)
  , (/=)
  , (==)
  , ($)
  , (>=>)
  , (<$>)
  , (*)
  , (<>)
  , (=<<)
  , (>>=)
  , show
  , pure
  , mempty
  )
-- TODO: find out where this is in the Contract namespace
import Contract.PlutusData (Redeemer(..))
import Contract.Numeric.BigNum as BigNum

-- import Contract.Scripts (applyArgs)
import MLabsPlutusTemplate.Scripts (password_validator, simple_policy)
import Data.Function.Uncurried (Fn1, mkFn1, Fn2, mkFn2, Fn3, mkFn3, runFn2, mkFn4, Fn4)
import Data.Array (cons, find, filter, head)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.Prelude (Either(..), Effect(..), Maybe(..), liftM, wrap, unwrap)
import Contract.Monad (Contract, runContract)
import Contract.Config
  ( testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  ) as Config
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV1FromEnvelope)
import Control.Monad.Error.Class (class MonadThrow, throwError, liftMaybe)
import Effect.Exception (Error, error)
import Contract.PlutusData
  ( class ToData
  , toData
  , class FromData
  , PlutusData(Bytes, Constr)
  , unitDatum
  , unitRedeemer
  )
import Contract.Transaction
  ( TransactionHash
  , _input
  , submitTxFromConstraints
  , lookupTxHash
  , awaitTxConfirmed
  )
import Contract.Log (logInfo')
import Contract.Scripts (MintingPolicy(..), Validator(..), validatorHash)
import Contract.TxConstraints (TxConstraints)
import Contract.Credential (Credential(PubKeyCredential))
import Contract.TxConstraints
  ( mustPayToScript
  , mustSpendAtLeast
  , mustSpendScriptOutput
  , mustMintValueWithRedeemer
  , mustPayToPubKey
  , DatumPresence(DatumWitness)
  ) as Constraints
import Contract.Address (scriptHashAddress)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Contract.Wallet (ownPaymentPubKeyHashes)
import Data.Lens (view)
import Data.BigInt (fromString, fromInt) as BigInt
import Contract.ScriptLookups (ScriptLookups, validator, unspentOutputs, mintingPolicy) as Lookups
import Contract.Value (CurrencySymbol, TokenName, mkTokenName)
import Contract.Value (lovelaceValueOf, scriptCurrencySymbol, singleton) as Value
import Effect.Aff (Aff, launchAff_)
import Control.Promise (Promise, fromAff)
import Data.Tuple.Nested (type (/\), (/\))
import Contract.Address (getWalletAddresses, getWalletCollateral)
import Contract.Config (ContractParams)
import Contract.Monad (Contract, launchAff_, runContract)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Effect.Unsafe (unsafePerformEffect)
import Aeson
  ( class DecodeAeson
  , decodeAeson
  , parseJsonStringToAeson
  )
import Data.Either (hush)
import Data.Nullable (Nullable, toNullable)
-- ply-ctl
import Ply.Apply
import Ply.Reify
import Ply.TypeList
import Ply.Typename
import Ply.Types
import Utils

{-
A type declaration like this is needed for ply-ctl integration.
See the ply-ctl documentation for more elaborate examples:
https://github.com/mlabs-haskell/ply-ctl
-}
type PasswordValidator =
  TypedScript
    ValidatorRole -- We must annotate the Role of the script (ValidatorRole/MintingPolicyRole)
    (Cons (AsData ByteArray) Nil) -- A TypeLevel list of the arguments to the validator-construction function

-- The validator, constructed by applying a password String argument
passwordValidator :: Fn1 Password (Contract Validator)
passwordValidator = mkFn1 $ \str -> do
  -- First, we have to decode the JSON String that represents the script envelope to Aeson
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson password_validator
  -- Next, we decode the Aeson into a TypedScriptEnvelope
  envelope <-
    liftErr ("Error reading validator envelope: \n" <> password_validator) <<< hush $
      decodeAeson aeson :: _ TypedScriptEnvelope
  -- Next, we use ply-ctl's reifiction machinery to read the typed envelope.
  -- This will throw an error of the argument types or role  in the envelope do not correspond to
  -- the types we declared in our PasswordValidator type (see above)
  tvalidator <-
    liftErr "Error decoding password envelope" <<< hush $
      reifyTypedScript envelope :: _ PasswordValidator
  -- Converts an ascii string to a byteArray
  pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii str
  -- We use ply-ctl's `applyParam` function to apply our ByteArray argument to the
  -- script read from the envelope
  case applyParam tvalidator pw of
    Left err -> throwError (error $ show err)
    Right applied -> pure <<< Validator <<< toPlutusScript $ applied

{-
These are wrappers around the `payToPassword'` and `spendFromPassword'` endpoints which allow
them to be called like ordinary JS functions - e.g. `payToPassword(arg1,arg2,arg3)`

NOTE: If you do not use the mkFnX wrappers, you will likely get an error, but it appears that
      un-wrapped functions can still be called in `payToPassword(arg1)(arg2)(arg3)` style.
-}
payToPassword :: Fn3 ContractParams Password AdaValue (Promise TransactionHash)
payToPassword = mkFn3 $ \cfg pw adaVal -> execContract' cfg (payToPassword' pw adaVal)

spendFromPassword :: Fn3 ContractParams TransactionHash Password Unit
spendFromPassword = mkFn3 $ \cfg txhash pwStr ->
  execContract cfg $ spendFromPassword' pwStr txhash

{-
Password Validator endpoints. These were adapted from the alwaysSucceeds examples in
the CTL `examples` directory, which contains many other example endpoints and can be viewed here:
https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/examples
-}
payToPassword'
  :: Password
  -> AdaValue
  -> Contract TransactionHash -- txhash : Uint8Array
payToPassword' pwStr adaValStr = do
  logInfo' "Paying to Password validator..."
  validator <- passwordValidator pwStr
  adaVal <- liftErr "Error: Invalid ada value string" $
    BigInt.fromString adaValStr
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
  :: Password
  -> TransactionHash
  -> Contract Unit
spendFromPassword' pwStr txId = do
  pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii pwStr
  validator <- passwordValidator pwStr
  let
    vhash = validatorHash validator

    scriptAddress =
      scriptHashAddress vhash Nothing

  utxos <- utxosAt scriptAddress
  txInput <-
    liftM
      ( error
          ( "The id "
              <> show txId
              <> " does not have output locked at: "
              <> show scriptAddress
          )
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
