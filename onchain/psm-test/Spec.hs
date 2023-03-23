{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Plutarch (
  Config (Config, tracingMode),
  TracingMode (DoTracing),
  pcon,
  (#),
 )
import Plutarch.Api.V1.Value (PTokenName (PTokenName))
import Plutarch.ExampleContracts (
  alwaysSucceeds,
  mkPasswordValidator,
  mkSimpleMP,
 )
import Plutarch.Prelude (pconstant, pdata)
import Plutus.Model (
  DatumMode (HashDatum),
  Run,
  TypedPolicy,
  TypedValidator,
  adaValue,
  boxAt,
  defaultBabbageV1,
  logBalanceSheet,
  mintValue,
  newUser,
  payToKey,
  payToScript,
  scriptCurrencySymbol,
  spend,
  spendBox,
  submitTx,
  testNoErrorsTrace,
  userSpend,
  withSpend,
 )
import Plutus.Model.Validator.V1 (
  mkTypedPolicyPlutarch,
  mkTypedValidatorPlutarch,
 )
import PlutusLedgerApi.V1 (
  BuiltinByteString,
  PubKeyHash,
  TokenName (TokenName),
  singleton,
 )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFail)

import Types (MintRedeemer (..))

main :: IO ()
main =
  defaultMain $
    testGroup
      "Example PSM Tests"
      [ testGroup "alwaysSucceeds" [mkTest "happy alwaysSucceeds" simpleTest]
      , testGroup
          "password validator"
          [ mkTest "happy password" pwTestHappy
          , mkTestShouldFail "" pwTestShouldFail
          ]
      , testGroup
          "minting policy"
          [ mkTest "happy tokens" tokenTestHappy
          , mkTestShouldFail "should fail tokens" tokenTestShouldFail
          ]
      ]

-- Utilities for creating TestTrees from Run action
mkTest :: String -> Run a -> TestTree
mkTest testname t =
  testNoErrorsTrace -- This is how we make a TestTree out of a Run a
    (adaValue 10000) -- The Value distributed to the admin user upon creation of the mock chain
    defaultBabbageV1 -- The configuration
    testname
    t

mkTestShouldFail :: String -> Run a -> TestTree
mkTestShouldFail s = expectFail . mkTest s

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

-- Example GUI Script Tests

type PasswordValidator = TypedValidator () BuiltinByteString

pwValidator :: ByteString -> PasswordValidator
pwValidator bs = case mkTypedValidatorPlutarch (Config {tracingMode = DoTracing}) (mkPasswordValidator # pdata (pconstant bs)) of
  Left e -> error (T.unpack e)
  Right v -> v

type Password = String

{- | Returns the validator created by applying the Password argument
     and the PKH of a user
-}
payPWScript :: Password -> Run (PasswordValidator, PubKeyHash)
payPWScript pw = do
  [u1, u2, _] <- setupUsers
  let validator = pwValidator (fromString pw)
  withSpend u1 (adaValue 500) $ \u1Spend -> do
    let tx =
          userSpend u1Spend
            <> payToScript validator (HashDatum ()) (adaValue 500)
    submitTx u1 tx
  logBalanceSheet
  pure (validator, u2)

spendPWScript :: PasswordValidator -> PubKeyHash -> Password -> Run ()
spendPWScript validator pkh pw = do
  [scriptBox] <- boxAt validator
  let tx =
        spendBox validator (fromString pw) scriptBox
          <> payToKey pkh (adaValue 500)
  submitTx pkh tx
  logBalanceSheet

pwTestHappy :: Run ()
pwTestHappy = do
  (v, u2) <- payPWScript "password"
  spendPWScript v u2 "password"

pwTestShouldFail :: Run ()
pwTestShouldFail = do
  (v, u2) <- payPWScript "password"
  spendPWScript v u2 "donut"

type TokenPolicy = TypedPolicy MintRedeemer

simplePolicy :: ByteString -> TokenPolicy
simplePolicy tn = case mkTypedPolicyPlutarch (Config {tracingMode = DoTracing}) $ mkSimpleMP # pdata (pcon (PTokenName $ pconstant tn)) of
  Left e -> error (T.unpack e)
  Right v -> v

type TokenString = String

simpleMintTokens :: TokenString -> Run (TokenPolicy, TokenString, PubKeyHash)
simpleMintTokens tkstr = do
  [u1, _, _] <- setupUsers
  let tkByteStr :: forall x. IsString x => x
      tkByteStr = fromString tkstr
      policy = simplePolicy tkByteStr
      cs = scriptCurrencySymbol policy
      valToMint = singleton cs (TokenName tkByteStr) 100
      tx =
        mintValue policy MintTokens valToMint
          <> payToKey u1 valToMint
  submitTx u1 tx
  logBalanceSheet
  pure (policy, tkstr, u1)

simpleBurnTokens :: TokenPolicy -> TokenString -> PubKeyHash -> MintRedeemer -> Run ()
simpleBurnTokens policy tkstr pkh rdmr = do
  let cs = scriptCurrencySymbol policy
      mkToken = singleton cs (TokenName $ fromString tkstr)
      valToBurn = mkToken (-50)
  uspend <- spend pkh (mkToken 50)
  let tx =
        mintValue policy rdmr valToBurn
          <> userSpend uspend
  submitTx pkh tx
  logBalanceSheet

tokenTestHappy :: Run ()
tokenTestHappy =
  simpleMintTokens "leetcoin" >>= \(policy, tkstr, pkh) ->
    simpleBurnTokens policy tkstr pkh BurnTokens

tokenTestShouldFail :: Run ()
tokenTestShouldFail =
  simpleMintTokens "leetcoin" >>= \(policy, tkstr, pkh) ->
    simpleBurnTokens policy tkstr pkh MintTokens
