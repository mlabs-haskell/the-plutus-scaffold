module MlabsPlutusTemplate.Api
  ( payToPassword
  , spendFromPassword
  , mintTokens
  , burnTokens
  , insertPWTXHash
  , lookupTXHashByPW
  , PWTXHash
  , module Config
  ) where

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
import Data.Function.Uncurried (Fn1, mkFn1, Fn2, mkFn2, Fn3, mkFn3, runFn2, mkFn4, Fn4)

import MLabsPlutusTemplate.Scripts (password_validator, simple_policy)

-- For this import we need newer ctl revision, that module seems useful
-- import Contract.JsSdk
--   ( runContractJS
--   )

-- TODO: find out where this is in the Contract namespace
import Contract.PlutusData (Redeemer(..))
import Contract.Numeric.BigNum as BigNum

-- import Contract.Scripts (applyArgs)
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

{-
   Utility Functions
-}

-- Turns an ascii string into a token name.
stringToTokenName :: String -> Maybe TokenName
stringToTokenName = byteArrayFromAscii >=> mkTokenName

type Password = String
type AdaValue = String

-- Gets the CurrencySymbol corresponding to a minting policy
mkCurrencySymbol
  :: Contract MintingPolicy
  -> Contract (MintingPolicy /\ CurrencySymbol)
mkCurrencySymbol mintingPolicy = do
  mp <- mintingPolicy
  cs <- liftErr "Cannot get cs" $ Value.scriptCurrencySymbol mp
  pure (mp /\ cs)

{-
Utility for throwing Errors in the Contract Monad
(Contract has a MonadThrow Error instance)
-}
liftErr :: forall m a. MonadThrow Error m => String -> Maybe a -> m a
liftErr msg a = liftMaybe (error msg) a

{-
Execute an action in the Contract Monad that returns some value.
The returned value is wrapped in a Promise, and can be treated like
a normal JavaScript promise in JS/TS code
-}
execContract' :: forall a. ContractParams -> Contract a -> Promise a
execContract' cfg contract = unsafePerformEffect $ fromAff $
 runContract cfg contract

{-
Execute an action in the Contract Monad that does not return a value.
`unsafePerformEffect` is OK here; practically it only serves to let you
write `f(arg)` instead of `f(arg)()`
See the "Calling PureScript from JavaScript" addendum here:
https://book.purescript.org/chapter10.html
for more information
-}
execContract :: ContractParams -> Contract Unit -> Unit
execContract cfg contract = unsafePerformEffect $ launchAff_ do
  runContract cfg contract

{-
In order to spend funds locked at the Password validator, we need the
TX hash of the TX that locked the funds. We use an array of PWTXHash records
instead of a Map to better integrate w/ TypeScript: An array of records
neatly corresponds to a JS/TS array of objects, whereas typing a Map would be
significantly more complicated
-}
newtype PWTXHash = PWTXHash
  { password :: String
  , txHash :: TransactionHash
  }
derive instance Newtype PWTXHash _

lookupTXHashByPW :: Fn2 String (Array PWTXHash) (Nullable PWTXHash)
lookupTXHashByPW = mkFn2 $ \str arr -> toNullable $ find (\x -> (unwrap x).password == str) arr

insertPWTXHash :: Fn3 String TransactionHash (Array PWTXHash) (Array PWTXHash)
insertPWTXHash = mkFn3 $ \str txhash arr ->
  cons (wrap { password: str, txHash: txhash })
    <<< deletePWTXHash str
    $ arr

deletePWTXHash :: String -> Array PWTXHash -> Array PWTXHash
deletePWTXHash str arr = filter (\x -> (unwrap x).password /= str) arr

{-
   Password Validator Offchain Logic
-}


{-
A type declaration like this is needed for ply-ctl integration.
See the ply-ctl documentation for more elaborate examples:
https://github.com/mlabs-haskell/ply-ctl
-}
type PasswordValidator =
  TypedScript
    ValidatorRole -- We must annotate the Role of the script (ValidatorRole/MintingPolicyRole)
      (Cons (AsData ByteArray) Nil) -- A TypeLevel list of the arguments to the validator-construction function

{-
These are wrappers around the `payToPassword'` and `spendFromPassword'` endpoints which allow
them to be called like ordinary JS functions - e.g. `payToPassword(arg1,arg2,arg3)`

NOTE: If you do not use the mkFnX wrappers, you will likely get an error, but it appears that
      un-wrapped functions can still be called in `payToPassword(arg1)(arg2)(arg3)` style.
-}
payToPassword :: Fn3 ContractParams Password AdaValue (Promise TransactionHash)
payToPassword = mkFn3 $ \cfg pw adaVal -> execContract' cfg (payToPassword' pw adaVal)

spendFromPassword :: Fn4 ContractParams TransactionHash Password AdaValue Unit
spendFromPassword = mkFn4 $ \cfg txhash pwStr valStr ->
  execContract cfg $ spendFromPassword' pwStr  valStr txhash

-- The validator, constructed by applying a password String argument
passwordValidator :: Fn1 Password (Contract Validator)
passwordValidator = mkFn1 $ \str -> do
  -- First, we have to decode the JSON String that represents the script envelope to Aeson
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson password_validator
  -- Next, we decode the Aeson into a TypedScriptEnvelope
  envelope <- liftErr ("Error reading validator envelope: \n" <> password_validator) <<< hush $
    decodeAeson aeson :: _ TypedScriptEnvelope
  -- Next, we use ply-ctl's reifiction machinery to read the typed envelope.
  -- This will throw an error of the argument types or role  in the envelope do not correspond to
  -- the types we declared in our PasswordValidator type (see above)
  tvalidator <- liftErr "Error decoding password envelope" <<< hush $
    reifyTypedScript envelope :: _ PasswordValidator
  -- Converts an ascii string to a byteArray
  pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii str
  -- We use ply-ctl's `applyParam` function to apply our ByteArray argument to the
  -- script read from the envelope
  case applyParam tvalidator pw  of
    Left err -> throwError (error $ show err)
    Right applied -> pure <<< Validator <<< toPlutusScript $ applied

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
  -> AdaValue
  -> TransactionHash
  -> Contract Unit
spendFromPassword' pwStr valStr txId = do
  pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii pwStr
  adaVal <- liftErr "Error: Invalid ada value string" $
             BigInt.fromString valStr
  ownPKHs <- ownPaymentPubKeyHashes
  walletPKH <- liftErr "no PKHs in wallet!" $ head ownPKHs
  validator <- passwordValidator pwStr
  let
    toSpend = Value.lovelaceValueOf $ adaVal * BigInt.fromInt 1_000_000
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
      <> Constraints.mustPayToPubKey walletPKH toSpend
      <> Constraints.mustSpendAtLeast toSpend

  spendTxId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed spendTxId
  logInfo' "Successfully spent locked values."

{-
   Simple minting policy offchain logic
-}

{-
Type declaration for ply-ctl compatibility. See the comments on the PasswordValidator
type above for an explanation of how this works.
-}
type SimplePolicy =
  TypedScript
    MintingPolicyRole
      (Cons (AsData TokenName) Nil)

type TokenString = String
type MintAmount = String


{-
Function that constructs a MintingPolicy from a String. This follows the same
pattern as `passwordValidator` above; see the comments there for more information
on how this works.
-}
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

{-
Because our contract expects & requires a redeemer, we must define a
type that corresponds to the Plutarch `PMintRedeemer` type that we defined in
onchain/src/Plutarch/ExampleContracts.hs

It is *extremely important* that the onchain representation of this type exactly
matches the onchain representation of `PMintRedeemer` - a mismatch will cause errors
which are not easy to diagnose, especially if the mismatched types are components of a
larger type.

Here, ensuring that the representations match requires only that the ConstrIndex fields in the
`ToData` instance indicate the correct order of constructors in the PMintRedeemer sum type.

NOTE: Pay attention to the deriving strategy used on the original Plutarch/Plutus types! A type
      that obtains its `ToData` (or Plutarch equivalent) instance from GeneralizedNewtypeDeriving
      or via PNewType has a different onchain structure than one constructed in other ways:
      A type w/ `newtype-ey` instances has the same onchain representation as its underlying type.

NOTE: A type can have a `newtype-ey` ToData/FromData instance *without being a newtype in Haskell
      or Plutarch*, so you have to examine the instance or deriving clause for the original
      type to understand how to construct the corresponding PureScript type.
-}
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

{-
Wrapped endpoints for SimplePolicy. See `payToPassword` and `spendFromPassword`
above for an explanation of the role of these wrappers & why we need them.
-}
mintTokens :: Fn3 ContractParams TokenString MintAmount Unit
mintTokens  = mkFn3 $ \cfg tkStr amt -> execContract cfg $ mintTokens' tkStr amt

burnTokens :: Fn3 ContractParams TokenString MintAmount Unit
burnTokens = mkFn3 $ \cfg tkStr amt -> execContract cfg $ burnTokens' tkStr amt

{-
The endpoints for SimplePolicy. Adapted from the `AlwaysMints` CTL example.
More example contracts can be found here:
https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/examples
-}
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
