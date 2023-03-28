module MlabsPlutusTemplate.Api
  ( square
  , payToPassword
  , spendFromPassword
  , mintTokens
  , burnTokens
  , insertPWTXHash
  , lookupTXHashByPW
  , deletePWTXHash
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
  , show
  , pure
  , mempty
  )
import Data.Function.Uncurried (Fn1, mkFn1, Fn2, mkFn2)

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
import Contract.Config (testnetNamiConfig)
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

square :: Fn1 Int Int
square = mkFn1 $ \n -> n * n

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

-- there should be a ToData instance for this *somewhere* but I can't find it
byteArrayToData :: ByteArray -> PlutusData
byteArrayToData = Bytes

liftErr :: forall m a. MonadThrow Error m => String -> Maybe a -> m a
liftErr msg a = liftMaybe (error msg) a

execContract' :: forall a. Contract a -> (Effect (Promise a))
execContract' contract = fromAff $ runContract testnetNamiConfig contract

execContract :: Contract Unit -> Effect Unit
execContract contract = launchAff_ do
  runContract testnetNamiConfig contract

newtype PWTXHash = PWTXHash
  { password :: String
  , txhash :: TransactionHash
  }

derive instance Newtype PWTXHash _

lookupTXHashByPW :: String -> Array PWTXHash -> Maybe PWTXHash
lookupTXHashByPW str arr = find (\x -> (unwrap x).password == str) arr

insertPWTXHash :: String -> TransactionHash -> Array PWTXHash -> Array PWTXHash
insertPWTXHash str txhash arr =
  cons (wrap { password: str, txhash: txhash })
    <<< deletePWTXHash str
    $ arr

deletePWTXHash :: String -> Array PWTXHash -> Array PWTXHash
deletePWTXHash str arr = filter (\x -> (unwrap x).password /= str) arr

{-
   Password Validator Offchain Logic
-}

payToPassword :: Fn2 Password AdaValue (Effect (Promise TransactionHash))
payToPassword = mkFn2 $ \pw adaVal -> execContract' (payToPassword' pw adaVal)

spendFromPassword :: Fn2 TransactionHash String (Effect Unit)
spendFromPassword = mkFn2 $ \txhash pwStr ->
  execContract $ spendFromPassword' pwStr txhash

-- The validator, constructed by applying a password String argument
passwordValidator :: Password -> Contract Validator
passwordValidator str = do
  envelope <- liftErr "Error decoding password validator" $
    decodeTextEnvelope password_validator
  validator <- liftErr "Error decoding password envelope" $
    plutusScriptV1FromEnvelope envelope
  pw <- liftErr "Error: Non-ascii chars in password" $
    byteArrayToData <$> byteArrayFromAscii str
  case applyArgs validator [ pw ] of
    Left err -> throwError (error $ show err)
    Right applied -> pure $ Validator applied

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

  submitTxFromConstraints lookups constraints

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

type TokenString = String
type MintAmount = String

simplePolicy :: TokenString -> Contract MintingPolicy
simplePolicy str = do
  envelope <- liftErr "Error decoding simple policy envelope" $
    decodeTextEnvelope simple_policy
  policy <- liftErr "Error converting policy envelope to script" $
    plutusScriptV1FromEnvelope envelope
  tkNm <- liftErr "Error: Invalid tokenName" $
    stringToTokenName str
  case applyArgs policy [ toData tkNm ] of
    Left err -> throwError (error $ show err)
    Right applied -> pure $ PlutusMintingPolicy applied

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

mintTokens :: TokenString -> MintAmount -> Effect Unit
mintTokens tkStr amt = execContract $ mintTokens' tkStr amt

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

burnTokens :: TokenString -> MintAmount -> Effect Unit
burnTokens tkStr amt = execContract $ burnTokens' tkStr amt

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
