{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell, LambdaCase #-}
module Main (main) where

import Plutarch.Prelude (pconstant,(#))
import Plutarch.ExampleContracts (alwaysSucceeds, nftMp)
import System.Environment (getArgs)

import Exporter

main :: IO ()
main = do
  dir <- head <$> getArgs
  runExporter dir $ do
    hash <- savePlutarchScript  "always_succeeds" alwaysSucceeds
    savePlutarchScript_ "nft_hash_applied" (nftMp # pconstant hash)
    savePlutarchScript_ "nft_no_hash_applied" nftMp
    -- savePlutusV1MintingPolicy_ "plutus_policy" some_v1_plutus_policy
