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

module Tokenomia.Vesting.CLI (vestFunds) where

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

vestFunds :: (MonadMask m,MonadIO m, MonadReader Environment m)  => m ()
vestFunds = do 
    liftIO $ echo "Select the token's owner wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just ownerWallet -> do
                liftIO $ echo "Select the investor's wallet" 
                Wallet.select
                    >>= \case 
                        Nothing -> liftIO $ print "No Wallet Registered !"
                        Just investorWallet@Wallet{} -> do 
                            liftIO $ echo (show investorWallet )