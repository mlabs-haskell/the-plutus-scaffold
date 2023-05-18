-- TAG: DEPLOYMENT_CTLRUNTIME
-- This module defines ContractParams configs (which importantly control ctl-runtime)
-- that link to the public MLabs ctl runtime instances!
-- 
-- If you happen to host your runtime yourself, here's where you can tell ctl to use it.
module DeploymentContractParams
  ( testnetNamiConfig
  , testnetGeroConfig
  , testnetFlintConfig
  , testnetEternlConfig
  , testnetLodeConfig
  , testnetNuFiConfig
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Data.UInt as UInt

import Contract.Config
  ( ServerConfig
  , ContractParams
  , WalletSpec
      ( ConnectToNami
      , ConnectToGero
      , ConnectToLode
      , ConnectToEternl
      , ConnectToFlint
      , ConnectToNuFi
      )
  , mkCtlBackendParams
  , NetworkId(TestnetId)
  , LogLevel(Trace)
  , emptyHooks
  )

publicMlabsOgmiosWsConfig :: ServerConfig
publicMlabsOgmiosWsConfig =
  { port: UInt.fromInt 1337
  , host: "ogmios.preview.ctl-runtime.staging.mlabs.city"
  , secure: false
  , path: Nothing
  }

publicMlabsKupoServerConfig :: ServerConfig
publicMlabsKupoServerConfig =
  { port: UInt.fromInt 4008
  , host: "kupo.preview.ctl-runtime.staging.mlabs.city"
  , secure: false
  , path: Nothing -- TODO: `Just "kupo"`? what's the relation to webpack's kupo proxy?
  }

testnetConfig :: ContractParams
testnetConfig =
  { backendParams: mkCtlBackendParams
      { ogmiosConfig: publicMlabsOgmiosWsConfig
      , kupoConfig: publicMlabsKupoServerConfig
      }
  , networkId: TestnetId
  , walletSpec: Nothing
  , logLevel: Trace
  , customLogger: Nothing
  , suppressLogs: false
  , hooks: emptyHooks
  }

testnetNamiConfig :: ContractParams
testnetNamiConfig = testnetConfig { walletSpec = Just ConnectToNami }

testnetGeroConfig :: ContractParams
testnetGeroConfig = testnetConfig { walletSpec = Just ConnectToGero }

testnetFlintConfig :: ContractParams
testnetFlintConfig = testnetConfig { walletSpec = Just ConnectToFlint }

testnetEternlConfig :: ContractParams
testnetEternlConfig = testnetConfig { walletSpec = Just ConnectToEternl }

testnetLodeConfig :: ContractParams
testnetLodeConfig = testnetConfig { walletSpec = Just ConnectToLode }

testnetNuFiConfig :: ContractParams
testnetNuFiConfig = testnetConfig { walletSpec = Just ConnectToNuFi }
