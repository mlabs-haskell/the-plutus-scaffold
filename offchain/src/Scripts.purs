module Scripts
  ( always_succeeds
  , nft_hash_applied
  , nft_no_hash_applied
  , password_validator
  , simple_policy
  , always_succeeds_envelope
  , nft_hash_applied_envelope
  , nft_no_hash_applied_envelope
  , password_validator_envelope
  , simple_policy_envelope
  ) where



import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap,unwrap)
import Contract.TextEnvelope
import Ctl.Internal.Types.Scripts (PlutusScript)
  
parseScript :: String -> Maybe PlutusScript
parseScript myscript = case unwrap <$> decodeTextEnvelope myscript of
  Just e -> case e.type_ of
    PlutusScriptV1 -> plutusScriptV1FromEnvelope (wrap e)
    PlutusScriptV2 -> plutusScriptV2FromEnvelope (wrap e)
    _              -> Nothing
  Nothing -> Nothing

foreign import always_succeeds_envelope :: String
always_succeeds :: Maybe PlutusScript
always_succeeds = parseScript always_succeeds_envelope
foreign import nft_hash_applied_envelope :: String
nft_hash_applied :: Maybe PlutusScript
nft_hash_applied = parseScript nft_hash_applied_envelope
foreign import nft_no_hash_applied_envelope :: String
nft_no_hash_applied :: Maybe PlutusScript
nft_no_hash_applied = parseScript nft_no_hash_applied_envelope
foreign import password_validator_envelope :: String
password_validator :: Maybe PlutusScript
password_validator = parseScript password_validator_envelope
foreign import simple_policy_envelope :: String
simple_policy :: Maybe PlutusScript
simple_policy = parseScript simple_policy_envelope
