
module CompiledScripts (scripts) where

import Contract.Prelude (bind)
import Foreign.Object (Object)

foreign import scripts :: Object String

-- decodePlutusScript = do
--   env <- decodeTextEnvelope
--   case env.type_ of 
--     PlutusScriptV1 -> Just $ plutusV1Script env.bytes
--     PlutusScriptV2 -> Just $ plutusV2Script env.bytes
--     PaymentSigningKeyShelleyed25519 -> Nothing
--     StakeSigningKeyShelleyed25519 -> Nothing
--     Other other -> Nothing
