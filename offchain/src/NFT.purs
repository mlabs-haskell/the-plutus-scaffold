module NFT (mintTokens, burnTokens) where

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
import Utils

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

{-
Function that constructs a MintingPolicy from a String. This follows the same
pattern as `passwordValidator` above; see the comments there for more information
on how this works.
-}
simplePolicy :: TokenString -> Contract MintingPolicy
simplePolicy str = do
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson simple_policy
  envelope <-
    liftErr ("Error decoding simple policy envelope: \n" <> simple_policy)
      <<< hush
      $ decodeAeson aeson :: _ TypedScriptEnvelope
  tpolicy <-
    liftErr "Error converting policy envelope to script"
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
mintTokens = mkFn3 $ \cfg tkStr amt -> execContract cfg $ mintTokens' tkStr amt

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
