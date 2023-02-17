-- | This module, when bundled, executes the default contract in the browser or
-- | the Node.
module MLabsPlutusTemplate.Main (main) where

import Contract.Prelude

import Contract.Address (getWalletAddresses)
import Contract.Config as Contract.Config
import Contract.Monad as Contract.Monad

main :: Effect Unit
main = Contract.Monad.launchAff_
  $ void
  $ Contract.Monad.runContract Contract.Config.testnetNamiConfig
  $ getWalletAddresses
