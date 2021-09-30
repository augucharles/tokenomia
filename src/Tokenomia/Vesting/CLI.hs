{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Vesting.CLI where

import           Prelude
import           Shh

import           Control.Monad.Reader
import           Control.Monad.Catch ( MonadMask ) 


import qualified Data.Text as T



import           Ledger hiding (value)
import           Ledger.Value
import           Plutus.V1.Ledger.Ada


import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import qualified Tokenomia.Wallet.CLI as Wallet


load SearchPath ["echo"]

vest :: (MonadMask m,MonadIO m, MonadReader Environment m)  => m ()