module LocalContractParams
  ( testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  ) where

import Contract.Config
  ( testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  ) as Config

import Contract.Config
  ( ServerConfig
  , ContractParams
  , mkCtlBackendParams
  , defaultOgmiosWsConfig
  )

import Data.Maybe (Maybe(Just, Nothing))
import Data.UInt as UInt

-- Alter kupo server config to point to kupo started with `nix run .#ctl-runtime`
-- 

localhostBackendParams = mkCtlBackendParams
  { ogmiosConfig: defaultOgmiosWsConfig
  , kupoConfig: localhostKupoServerConfig
  }

localhostKupoServerConfig :: ServerConfig
localhostKupoServerConfig =
  { port: UInt.fromInt 1442
  , host: "localhost"
  , secure: false
  , path: Nothing
  }

testnetNamiConfig :: ContractParams
testnetNamiConfig = Config.testnetNamiConfig { backendParams = localhostBackendParams }

testnetGeroConfig :: ContractParams
testnetGeroConfig = Config.testnetGeroConfig { backendParams = localhostBackendParams }

testnetFlintConfig :: ContractParams
testnetFlintConfig = Config.testnetFlintConfig { backendParams = localhostBackendParams }

testnetEternlConfig :: ContractParams
testnetEternlConfig = Config.testnetEternlConfig { backendParams = localhostBackendParams }

testnetLodeConfig :: ContractParams
testnetLodeConfig = Config.testnetLodeConfig { backendParams = localhostBackendParams }

testnetNuFiConfig :: ContractParams
testnetNuFiConfig = Config.testnetNuFiConfig { backendParams = localhostBackendParams }
