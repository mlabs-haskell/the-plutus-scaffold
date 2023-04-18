module PlyScripts (passwordValidator, simplePolicy) where

import Prelude (bind, pure, show, ($), (<<<), (<>))
import Scripts (password_validator, simple_policy)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Contract.Prelude (Either(..))
import Contract.Monad (Contract)
import Effect.Exception (error)
import Contract.Scripts (MintingPolicy(..), Validator(..))
import Contract.Value (TokenName)
import Aeson
  ( decodeAeson
  , parseJsonStringToAeson
  )
import Data.Either (hush)
import Control.Monad.Error.Class (throwError)
import Ply.Apply (applyParam)
import Ply.Reify (reifyTypedScript)
import Ply.TypeList (Cons, Nil)

import Ply.Types (AsData, MintingPolicyRole, TypedScript, TypedScriptEnvelope, ValidatorRole, toPlutusScript)
import Utils (liftErr)

{-
A type declaration like this is needed for ply-ctl integration.
See the ply-ctl documentation for more elaborate examples:
https://github.com/mlabs-haskell/ply-ctl
-}
type PasswordValidator =
  TypedScript
    ValidatorRole -- We must annotate the Role of the script (ValidatorRole/MintingPolicyRole)
    (Cons (AsData ByteArray) Nil) -- A TypeLevel list of the arguments to the validator-construction function

-- The validator, constructed by applying a password String argument
passwordValidator :: ByteArray -> Contract Validator
passwordValidator pw = do
  -- First, we have to decode the JSON String that represents the script envelope to Aeson
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson password_validator
  -- Next, we decode the Aeson into a TypedScriptEnvelope
  envelope <-
    liftErr ("Error reading validator envelope: \n" <> password_validator) <<< hush $
      decodeAeson aeson :: _ TypedScriptEnvelope
  -- Next, we use ply-ctl's reifiction machinery to read the typed envelope.
  -- This will throw an error of the argument types or role  in the envelope do not correspond to
  -- the types we declared in our PasswordValidator type (see above)
  tvalidator <-
    liftErr "Error decoding password envelope" <<< hush $
      reifyTypedScript envelope :: _ PasswordValidator
  -- Converts an ascii string to a byteArray
  -- pw <- liftErr "Error: Non-ascii chars in password" $ byteArrayFromAscii str
  -- We use ply-ctl's `applyParam` function to apply our ByteArray argument to the
  -- script read from the envelope
  case applyParam tvalidator pw of
    Left err -> throwError (error $ show err)
    Right applied -> pure <<< Validator <<< toPlutusScript $ applied

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
simplePolicy :: TokenName -> Contract MintingPolicy
simplePolicy tkNm = do
  aeson <- liftErr "invalid json" <<< hush $ parseJsonStringToAeson simple_policy
  envelope <-
    liftErr ("Error decoding simple policy envelope: \n" <> simple_policy)
      <<< hush
      $ decodeAeson aeson :: _ TypedScriptEnvelope
  tpolicy <-
    liftErr "Error converting policy envelope to script"
      <<< hush
      $ reifyTypedScript envelope :: _ SimplePolicy
  -- tkNm <- liftErr "Error: Invalid tokenName" (stringToTokenName str)
  case applyParam tpolicy tkNm of
    Left err -> throwError (error $ show err)
    Right applied -> pure <<< PlutusMintingPolicy <<< toPlutusScript $ applied
