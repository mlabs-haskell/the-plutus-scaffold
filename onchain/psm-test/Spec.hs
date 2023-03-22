{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Plutarch
import Plutarch.ExampleContracts
import Plutarch.Prelude
import Plutus.Model
import Plutus.Model.Validator.V1
import PlutusLedgerApi.V1
import Test.Tasty

import Plutarch.Api.V1.Value (PTokenName (PTokenName))
import Types

main :: IO ()
main =
  defaultMain $
    testNoErrorsTrace -- This is how we make a TestTree out of a Run a
      (adaValue 10000) -- The Value distributed to the admin user upon creation of the mock chain
      defaultBabbageV1 -- The configuration
      "simple psm test"
      simpleTest

-- You'll usually have a function like this to setup the initial users
-- to be used in your tests. For more complex scenarios, you might want to
-- wrap `Run` in a transformer stack that associates a name with each user,
-- e.g. StateT (Map Text PubKeyHash) Run
setupUsers :: Run [PubKeyHash]
setupUsers = replicateM 3 $ newUser $ adaValue 1000

-- To make use of a script, you must utilize the functions in Plutus.Model.Validator.V1/V2
-- to transform your Plutarch code or compiled PlutusTx code into a TypedValidator. (Same for minting policies)
--
-- `alwaysSucceeds` has open type variables for the Datum/Redeemer which we set to () here for
-- simplicity, however in a more fleshed out example you should indicate the Datum and Redeemer types
-- in the type signature here to take advantage of PSM's type-safety
--
-- NOTE: Make sure you're using the correct V1/V2 PSM functions for your script version!
myValidator :: TypedValidator () ()
myValidator = case mkTypedValidatorPlutarch (Config {tracingMode = DoTracing}) alwaysSucceeds of
  Left e -> error (T.unpack e)
  Right v -> v

-- A simple test that pays 500 ada from user1 to the alwaysSucceeds script.
-- As mentioned above, typically `Run` would sit at the bottom of a transformer stack
-- that e.g. keeps track of the expected contract state or associated a label with the
-- users (or something).
--
-- We return a PKH here to use it in a subsequent transaction, but again, in a more
-- complex example it'd be better to wrap Run in a transformer stack that keeps track of
-- the users
simplePayToScript :: Run PubKeyHash
simplePayToScript = do
  [u1, u2, _] <- setupUsers
  withSpend u1 (adaValue 500) $ \u1Spend -> do
    -- Transactions are a monoid and we construct them with <>
    let tx =
          userSpend u1Spend
            <> payToScript myValidator (HashDatum ()) (adaValue 500)
    -- submits the Tx to the chain after signing w/ u1's key
    submitTx u1 tx
  -- Prints balances in the log, generally useful.
  logBalanceSheet
  pure u2

-- Spends 500 ada from the alwaysSucceeds script to the PKH argument
simpleSpendScript :: PubKeyHash -> Run ()
simpleSpendScript pkh = do
  -- Working w/ a TxBox is generally more ergonomic than working w/
  -- a script directly. (Here there is only one box, in a more complex example
  -- you would likely use `withBox` and a filtering function to select the Box you want
  -- to spend at a given script)
  [scriptBox] <- boxAt myValidator
  let tx =
        spendBox myValidator () scriptBox
          <> payToKey pkh (adaValue 500)
  submitTx pkh tx
  logBalanceSheet

-- We should see two logged balance sheets:
--   1) The first one indicates that User1 has paid 500 ada to the script
--   2) The second one indicates the User2 has spent the script and paid 500 ada to themself
simpleTest :: Run ()
simpleTest = do
  u2 <- simplePayToScript
  simpleSpendScript u2

-- TODO: Remove, exists for ghci debugging
runTest :: Run a -> IO ()
runTest t =
  defaultMain $
    testNoErrorsTrace -- This is how we make a TestTree out of a Run a
      (adaValue 10000) -- The Value distributed to the admin user upon creation of the mock chain
      defaultBabbageV1 -- The configuration
      "simple psm test"
      t

payPWScript :: Run PubKeyHash
payPWScript = do
  [u1, u2, _] <- setupUsers
  let validator = pwValidator "password"
  withSpend u1 (adaValue 500) $ \u1Spend -> do
    let tx =
          userSpend u1Spend
            <> payToScript validator (HashDatum ()) (adaValue 500)
    submitTx u1 tx
  logBalanceSheet
  pure u2

spendPWScript :: PubKeyHash -> Run ()
spendPWScript pkh = do
  let validator = pwValidator "password"
  [scriptBox] <- boxAt validator
  let tx =
        spendBox validator "password" scriptBox
          <> payToKey pkh (adaValue 500)
  submitTx pkh tx
  logBalanceSheet

spendPWScriptBadPW :: PubKeyHash -> Run ()
spendPWScriptBadPW pkh = do
  let validator = pwValidator "password"
  [scriptBox] <- boxAt validator
  let tx =
        spendBox validator "donut" scriptBox
          <> payToKey pkh (adaValue 500)
  submitTx pkh tx
  logBalanceSheet

pwTest :: Run ()
pwTest = do
  u2 <- payPWScript
  spendPWScript u2

pwTestBadPW :: Run ()
pwTestBadPW = do
  u2 <- payPWScript
  spendPWScriptBadPW u2

pwValidator :: ByteString -> TypedValidator () BuiltinByteString
pwValidator bs = case mkTypedValidatorPlutarch (Config {tracingMode = DoTracing}) (mkPasswordValidator # pconstant bs) of
  Left e -> error (T.unpack e)
  Right v -> v

simplePolicy :: ByteString -> TypedPolicy MintRedeemer
simplePolicy tn = case mkTypedPolicyPlutarch (Config {tracingMode = DoTracing}) $ mkSimpleMP # pdata (pcon (PTokenName $ pconstant tn)) of
  Left e -> error (T.unpack e)
  Right v -> v

simpleMintTokens :: Run PubKeyHash
simpleMintTokens = do
  [u1, _, _] <- setupUsers
  let myPolicy = simplePolicy "leetcoin"
      cs = scriptCurrencySymbol myPolicy
      valToMint = singleton cs (TokenName "leetcoin") 100
      tx =
        mintValue myPolicy MintTokens valToMint
          <> payToKey u1 valToMint
  submitTx u1 tx
  logBalanceSheet
  pure u1

simpleBurnTokens :: PubKeyHash -> Run ()
simpleBurnTokens u1 = do
  let myPolicy = simplePolicy "leetcoin"
      cs = scriptCurrencySymbol myPolicy
      mkToken = singleton cs (TokenName "leetcoin")
      valToBurn = mkToken (-50)
  uspend <- spend u1 (mkToken 50)
  let tx =
        mintValue myPolicy BurnTokens valToBurn
          <> userSpend uspend
  submitTx u1 tx
  logBalanceSheet

tokenTest :: Run ()
tokenTest = simpleMintTokens >>= simpleBurnTokens
