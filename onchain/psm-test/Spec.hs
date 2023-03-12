import Control.Monad (replicateM)
import Plutarch (Config (..), TracingMode (DoTracing))
import Plutarch.ExampleContracts
import Plutus.Model
import Plutus.Model.Validator.V1 (mkTypedValidatorPlutarch)
import PlutusLedgerApi.V1 (PubKeyHash)

import qualified Data.Text as T
import Test.Tasty

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