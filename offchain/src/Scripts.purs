module Scripts
  ( always_succeeds
  , nft_hash_applied
  , nft_no_hash_applied
  , password_validator
  , simple_policy
  , always_succeeds_parsed
  , nft_hash_applied_parsed
  , nft_no_hash_applied_parsed
  , password_validator_parsed
  , simple_policy_parsed
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap, unwrap)
import Contract.TextEnvelope
import Ctl.Internal.Types.Scripts (PlutusScript)

foreign import always_succeeds :: String
foreign import nft_hash_applied :: String
foreign import nft_no_hash_applied :: String
foreign import password_validator :: String
foreign import simple_policy :: String

parseScript :: String -> Maybe PlutusScript
parseScript myscript = case unwrap <$> decodeTextEnvelope myscript of
  Just e -> case e.type_ of
    PlutusScriptV1 -> plutusScriptV1FromEnvelope (wrap e)
    PlutusScriptV2 -> plutusScriptV2FromEnvelope (wrap e)
    other -> Nothing
  Nothing -> Nothing

always_succeeds_parsed :: Maybe PlutusScript
always_succeeds_parsed = parseScript always_succeeds

nft_hash_applied_parsed :: Maybe PlutusScript
nft_hash_applied_parsed = parseScript nft_hash_applied

nft_no_hash_applied_parsed :: Maybe PlutusScript
nft_no_hash_applied_parsed = parseScript nft_no_hash_applied

password_validator_parsed :: Maybe PlutusScript
password_validator_parsed = parseScript password_validator

simple_policy_parsed :: Maybe PlutusScript
simple_policy_parsed = parseScript simple_policy