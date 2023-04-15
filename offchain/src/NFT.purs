module NFT (mintTokens, burnTokens) where

import Prelude (class Eq, class Ord, Unit, Void, bind, discard, ($), (<<<))
import Data.Generic.Rep (class Generic)
import Data.Function.Uncurried (Fn3, mkFn3)
import Contract.Monad (Contract)
import Contract.PlutusData
  ( class ToData
  , toData
  , PlutusData(Constr)
  , Redeemer(..)
  )
import Contract.Transaction
  ( submitTxFromConstraints
  , awaitTxConfirmed
  )
import Contract.Numeric.BigNum (fromInt) as BigNum
import Contract.Log (logInfo')
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints
  ( mustMintValueWithRedeemer
  ) as Constraints
import Contract.Value (TokenName, singleton)
import Data.Tuple.Nested ((/\))
import Data.BigInt (BigInt)
import Contract.ScriptLookups (ScriptLookups, mintingPolicy) as Lookups
import Contract.Config (ContractParams)
import Utils (execContract, mkCurrencySymbol)
import PlyScripts (simplePolicy)

{-
   Simple minting policy offchain logic
-}

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
mintTokens :: Fn3 ContractParams TokenName BigInt Unit
mintTokens = mkFn3 $ \cfg tkStr amt -> execContract cfg $ mintTokens' tkStr amt

burnTokens :: Fn3 ContractParams TokenName BigInt Unit
burnTokens = mkFn3 $ \cfg tkStr amt -> execContract cfg $ burnTokens' tkStr amt

{-
The endpoints for SimplePolicy. Adapted from the `AlwaysMints` CTL example.
More example contracts can be found here:
https://github.com/Plutonomicon/cardano-transaction-lib/tree/develop/examples
-}
mintTokens' :: TokenName -> BigInt -> Contract Unit
mintTokens' tn toMint = do
  -- toMint <- liftErr "Invalid MintAmount String" $ BigInt.fromString amt
  mp /\ cs <- mkCurrencySymbol $ simplePolicy tn
  -- tn <- liftErr "Invalid TokenName String" $ stringToTokenName tkStr
  let
    constraints :: TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer <<< toData $ MintTokens)
        $ singleton cs tn toMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

burnTokens' :: TokenName -> BigInt -> Contract Unit
burnTokens' tn toMint = do
  -- toMint <- liftErr "Invalid MintAmount String" $ BigInt.fromString amt
  mp /\ cs <- mkCurrencySymbol $ simplePolicy tn
  -- tn <- liftErr "Invalid TokenName String" $ stringToTokenName tkStr
  let
    constraints :: TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer <<< toData $ BurnTokens)
        $ singleton cs tn toMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"
