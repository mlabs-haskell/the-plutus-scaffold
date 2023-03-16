-- | This module is used to serve the E2E tests to the headless browser.
module MLabsPlutusTemplate.Test.E2E.Serve where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Config (ConfigParams, mainnetFlintConfig, mainnetGeroConfig, mainnetLodeConfig, mainnetNamiConfig, testnetEternlConfig, testnetFlintConfig, testnetGeroConfig, testnetLodeConfig, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract)
import Contract.Test.Cip30Mock (WalletMock(MockFlint, MockGero, MockNami, MockLode))
import Contract.Test.E2E (E2EConfigName, E2ETestName, addLinks, route)
import Data.Map (Map)
import Data.Map as Map

main :: Effect Unit
main = do
  addLinks configs tests
  route configs tests

configs :: Map E2EConfigName (ConfigParams () /\ Maybe WalletMock)
configs = Map.fromFoldable
  [ "nami" /\ testnetNamiConfig /\ Nothing
  , "gero" /\ testnetGeroConfig /\ Nothing
  , "flint" /\ testnetFlintConfig /\ Nothing
  , "eternl" /\ testnetEternlConfig /\ Nothing
  , "lode" /\ testnetLodeConfig /\ Nothing
  , "nami-mock" /\ testnetNamiConfig /\ Just MockNami
  , "gero-mock" /\ testnetGeroConfig /\ Just MockGero
  , "flint-mock" /\ testnetFlintConfig /\ Just MockFlint
  , "lode-mock" /\ testnetLodeConfig /\ Just MockLode
  -- Plutip cluster's network ID is set to mainnet:
  , "plutip-nami-mock" /\ mainnetNamiConfig /\ Just MockNami
  , "plutip-gero-mock" /\ mainnetGeroConfig /\ Just MockGero
  , "plutip-flint-mock" /\ mainnetFlintConfig /\ Just MockFlint
  , "plutip-lode-mock" /\ mainnetLodeConfig /\ Just MockLode
  ]

contract :: Contract () Unit
contract = do
  logInfo' "Welcome to CTL! Your wallet's payment PubKey hash is:"
  logInfo' <<< show =<< ownPaymentPubKeyHash

tests :: Map E2ETestName (Contract () Unit)
tests = Map.fromFoldable
  [ 
    "Contract" /\ contract
  -- Add more `Contract`s here
  ]
