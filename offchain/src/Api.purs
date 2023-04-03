module MlabsPlutusTemplate.Api
  ( payToPassword
  , spendFromPassword
  , mintTokens
  , burnTokens
  , insertPWTXHash
  , lookupTXHashByPW
  , PWTXHash
  ) where

import Prelude
  ( class Eq
  , class Ord
  , Void
  , bind
  , discard
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
import Data.Function.Uncurried (Fn1, mkFn1, Fn2, mkFn2, Fn3, mkFn3, runFn2)

import MLabsPlutusTemplate.Scripts (password_validator, simple_policy)

-- For this import we need newer ctl revision, that module seems useful
-- import Contract.JsSdk
--   ( runContractJS
--   )

-- TODO: find out where this is in the Contract namespace
import Ctl.Internal.Types.Redeemer (Redeemer(..))
import Ctl.Internal.Types.BigNum (fromInt) as BigNum

import Contract.Scripts (applyArgs)
import Data.Array (cons, find, filter, head)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.Prelude (Either(..), Effect(..), Maybe(..), liftM, wrap, unwrap)
import Contract.Monad (Contract, runContract)
import Contract.Config (testnetEternlConfig)
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
import Contract.TxConstraints
  ( mustPayToScript
  , mustSpendScriptOutput
  , mustMintValueWithRedeemer
  , DatumPresence(DatumWitness)
  ) as Constraints
import Contract.Address (scriptHashAddress)
import Contract.Utxos (utxosAt)
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
-- ply-ctl
import Ply.Apply
import Ply.Reify
import Ply.TypeList
import Ply.Typename
import Ply.Types

stringToTokenName :: String -> Maybe TokenName
stringToTokenName = byteArrayFromAscii >=> mkTokenName

type Password = String
type AdaValue = String

mkCurrencySymbol
  :: Contract MintingPolicy
  -> Contract (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  cs <- liftErr "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

liftErr :: forall m a. MonadThrow Error m => String -> Maybe a -> m a
liftErr msg a = liftMaybe (error msg) a

execContract' :: forall a. Contract a ->  (Promise a)
execContract' contract =  unsafePerformEffect $ fromAff $
 runContract testnetEternlConfig contract

execContract :: Contract Unit -> Unit
execContract contract = unsafePerformEffect $ launchAff_ do
  runContract testnetEternlConfig contract

newtype PWTXHash = PWTXHash
  { password :: String
  , txhash :: TransactionHash
  }

derive instance Newtype PWTXHash _

lookupTXHashByPW :: Fn2 String (Array PWTXHash) (Maybe PWTXHash)
lookupTXHashByPW = mkFn2 $ \str arr -> find (\x -> (unwrap x).password == str) arr

insertPWTXHash :: Fn3 String TransactionHash (Array PWTXHash) (Array PWTXHash)
insertPWTXHash = mkFn3 $ \str txhash arr ->
  cons (wrap { password: str, txhash: txhash })
    <<< deletePWTXHash str
    $ arr

deletePWTXHash :: String -> Array PWTXHash -> Array PWTXHash
deletePWTXHash str arr = filter (\x -> (unwrap x).password /= str) arr

{-
   Password Validator Offchain Logic
-}

type PasswordValidator =
  TypedScript
    ValidatorRole
      (Cons (AsData ByteArray) Nil)

payToPassword :: Fn2 Password AdaValue (Promise TransactionHash)
payToPassword = mkFn2 $ \pw adaVal -> execContract' (payToPassword' pw adaVal)

spendFromPassword :: Fn2 TransactionHash String Unit
spendFromPassword = mkFn2 $ \txhash pwStr ->
  execContract $ spendFromPassword' pwStr txhash

-- The validator, constructed by applying a password String argument
passwordValidator :: Fn1 Password (Contract Validator)
passwordValidator = mkFn1 $ \str -> do
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson password_validator
  envelope <- liftErr ("Error reading validator envelope: \n" <> password_validator) <<< hush $
    decodeAeson aeson :: _ TypedScriptEnvelope
  tvalidator <- liftErr "Error decoding password envelope" <<< hush $
    reifyTypedScript envelope :: _ PasswordValidator
  pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii str
  case applyParam tvalidator pw  of
    Left err -> throwError (error $ show err)
    Right applied -> pure <<< Validator <<< toPlutusScript $ applied

-- Pay to password endpoint
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
  -- awaitTxConfirmed txhash
  logInfo' "Successfully paid to password validator"
  pure txhash

-- Spend from password endpoint
spendFromPassword'
  :: String
  -> TransactionHash
  -> Contract Unit
spendFromPassword' pwStr txId = do
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
      Constraints.mustSpendScriptOutput txInput unitRedeemer

  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

{-
   Simple minting policy offchain logic
-}

type SimplePolicy =
  TypedScript
    MintingPolicyRole
      (Cons (AsData TokenName) Nil)

type TokenString = String
type MintAmount = String

simplePolicy :: TokenString -> Contract MintingPolicy
simplePolicy str = do
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson simple_policy
  envelope <- liftErr ("Error decoding simple policy envelope: \n" <> simple_policy)
              <<< hush
              $ decodeAeson aeson :: _ TypedScriptEnvelope
  tpolicy <- liftErr "Error converting policy envelope to script"
             <<< hush
             $ reifyTypedScript envelope :: _ SimplePolicy
  tkNm <- liftErr "Error: Invalid tokenName" (stringToTokenName str)
  case applyParam tpolicy tkNm of
    Left err -> throwError (error $ show err)
    Right applied -> pure <<< PlutusMintingPolicy <<< toPlutusScript $ applied

data MintRedeemer
  = MintTokens
  | BurnTokens

derive instance Generic MintRedeemer _
derive instance Eq MintRedeemer
derive instance Ord MintRedeemer

instance ToData MintRedeemer where
  toData = case _ of
    MintTokens -> Constr (BigNum.fromInt 0) []
    BurnTokens -> Constr (BigNum.fromInt 1) []

mintTokens :: Fn2 TokenString MintAmount Unit
mintTokens  = mkFn2 $ \tkStr amt -> execContract $ mintTokens' tkStr amt

mintTokens' :: TokenString -> MintAmount -> Contract Unit
mintTokens' tkStr amt = do
  toMint <- liftErr "Invalid MintAmount String" $ BigInt.fromString amt
  mp /\ cs <- mkCurrencySymbol $ simplePolicy tkStr
  tn <- liftErr "Invalid TokenName String" $ stringToTokenName tkStr
  let
    constraints :: TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer <<< toData $ MintTokens)
        $ Value.singleton cs tn toMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

burnTokens :: Fn2 TokenString MintAmount Unit
burnTokens = mkFn2 $ \tkStr amt -> execContract $ burnTokens' tkStr amt

burnTokens' :: TokenString -> MintAmount -> Contract Unit
burnTokens' tkStr amt = do
  toMint <- liftErr "Invalid MintAmount String" $ BigInt.fromString amt
  mp /\ cs <- mkCurrencySymbol $ simplePolicy tkStr
  tn <- liftErr "Invalid TokenName String" $ stringToTokenName tkStr
  let
    constraints :: TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer <<< toData $ BurnTokens)
        $ Value.singleton cs tn toMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"
