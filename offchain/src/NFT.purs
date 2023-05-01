module NFT (mintTokens, burnTokens) where

import Contract.Config (ContractParams)
import Contract.Log (logInfo')
import Contract.Monad (Contract, throwContractError)
import Contract.Numeric.BigNum (fromInt) as BigNum
import Contract.PlutusData (class ToData, toData, PlutusData(Constr), Redeemer(..))
import Contract.Prelude (either, pure)
import Contract.ScriptLookups (ScriptLookups, mintingPolicy) as Lookups
import Contract.Transaction (submitTxFromConstraints, awaitTxConfirmed)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustMintValueWithRedeemer) as Constraints
import Contract.Value (TokenName, scriptCurrencySymbol, singleton)
import Data.BigInt (BigInt)
import Data.Function.Uncurried (Fn3, mkFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import PlyScripts (makeSimplePolicy)
import Prelude (class Eq, class Ord, Unit, Void, bind, discard, ($), (<<<), negate)
import Utils (execContract)

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

SimplePolicy is a minting policy that:
  - accepts a redeemer which is a tag "Mint"/"Burn"
  - only validates if the redeemer matches the +/- sign of the minted amount of tokens
  - does not further validation
-}

{-
Contract submitting and awaiting transaction that mints simplePolicy tokens using "MintTokens" endpoint. 
-}
mintTokens' :: TokenName -> BigInt -> Contract Unit
mintTokens' tokenName tokenAmount = mintBurnTokensAux MintTokens tokenName tokenAmount

{-
Contract submitting and awaiting transaction that burns simplePolicy tokens using "BurnTokens" endpoint. 

First argument is a POSITIVE amount of tokens to BURN, which means the number is negated in the corresponding transaction field. 
-}
burnTokens' :: TokenName -> BigInt -> Contract Unit
burnTokens' tokenName tokenAmount = mintBurnTokensAux BurnTokens tokenName (negate tokenAmount)

{-
Contract submitting and awaiting a minting transaction for the simplePolicy minting policy.

"Mint"/"Burn" endpoints differ only in the submitted redeemer and token amount,
the below definition captures the shared part.
-}
mintBurnTokensAux ::  MintRedeemer -> TokenName -> BigInt -> Contract Unit
mintBurnTokensAux redeemer tokenName tokenAmount = do
  mp <- either throwContractError pure $ makeSimplePolicy tokenName
  cs <- maybe (throwContractError "Can't get currency symbol") pure $ 
    scriptCurrencySymbol mp

  let
    constraints :: TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (Redeemer <<< toData $ redeemer)
        $ singleton cs tokenName tokenAmount

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp

  txId <- submitTxFromConstraints lookups constraints

  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"