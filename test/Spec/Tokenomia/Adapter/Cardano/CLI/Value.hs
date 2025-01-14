{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
module Spec.Tokenomia.Adapter.Cardano.CLI.Value (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Text.InterpolatedString.Perl6 (qc)

import Plutus.V1.Ledger.Value ( singleton )

import Ledger.Ada ( lovelaceValueOf )

import Tokenomia.Adapter.Cardano.CLI.Value ()
import Tokenomia.Adapter.Cardano.CLI.Serialise ( FromCLI(fromCLI) ) 

tests :: TestTree
tests = testGroup "Value" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "fromCLI cardano-cli Value" $
     fromCLI [qc|  1344798 lovelace + 489999979900 bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.CLAP + TxOutDatumHashNone  |] 
      @?= lovelaceValueOf 1344798 <>  singleton "bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c" "CLAP" 489999979900]




