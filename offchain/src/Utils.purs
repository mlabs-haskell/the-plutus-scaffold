module Utils where

import Prelude (Unit, bind, pure, ($), (>=>))
import Ctl.Internal.Types.ByteArray (byteArrayFromAscii)
import Contract.Prelude (Maybe)
import Contract.Monad (Contract, runContract)
import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Effect.Exception (Error, error)
import Effect.Aff (launchAff_)
import Contract.Scripts (MintingPolicy)
import Contract.Value (CurrencySymbol, TokenName, mkTokenName)
import Contract.Value (scriptCurrencySymbol) as Value
import Control.Promise (Promise, fromAff)
import Data.Tuple.Nested (type (/\), (/\))
import Contract.Config (ContractParams)
import Effect.Unsafe (unsafePerformEffect)

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
