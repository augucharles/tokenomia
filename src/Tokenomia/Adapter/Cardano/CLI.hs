{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Adapter.Cardano.CLI
    ( -- Write 
      Internal.run_tx
    , Internal.register_minting_script_file
    , Internal.get_monetary_policy_path
    , Internal.register_shelley_wallet
    , Internal.remove_shelley_wallet
      -- Read 
    , Internal.query_registered_wallets
    , getUTxOs
    , Internal.query_tip
    , Internal.Wallet (..)
    , Internal.WalletAddress
    , Internal.Environment (..)) where


import           Control.Monad.Reader ( MonadIO, MonadReader )

import qualified Tokenomia.Adapter.Cardano.CLI.Internal as Internal
import           Tokenomia.Adapter.Cardano.CLI.UTxO ( UTxO )
import           Tokenomia.Adapter.Cardano.CLI.Serialise ( FromCLI(fromCLI) )


getUTxOs
  :: ( MonadIO m
     , MonadReader Internal.Environment m )
  => Internal.WalletAddress
  -> m [UTxO]
getUTxOs  a = fromCLI <$> Internal.query_utxo a

