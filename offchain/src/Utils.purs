module Utils where

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
import Ctl.Internal.Types.ByteArray (ByteArray, byteArrayFromAscii)

{-
   Utility Functions
-}

-- Turns an ascii string into a token name.
stringToTokenName :: String -> Maybe TokenName
stringToTokenName = byteArrayFromAscii >=> mkTokenName

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

type Password = String

type AdaValue = String

type TokenString = String

type MintAmount = String
