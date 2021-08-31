{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

-- | Implements a custom currency with a minting policy that allows
--   the minting of a fixed amount of units.

module Smartchain.Contract.CLAP.MonetaryPolicy(
    CurrencySchema
    , CurrencyError(..)
    , AsCurrencyError(..)
    -- , curPolicy
    -- * Actions etc
    , mintCLAPContract
    -- , currencySymbol
    -- * Simple minting policy currency
    , mintCLAPs
    ) where

import           Control.Lens
import           PlutusTx.Prelude       hiding (Monoid (..), Semigroup (..))

import           Plutus.Contract        as Contract
import           Plutus.Contract.Wallet (getUnspentOutput)

import           Ledger                 (PubKeyHash, TxId, TxOutRef (..), pubKeyHash, pubKeyHashAddress,
                                         scriptCurrencySymbol, txId)
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Contexts        as V
import           Ledger.Scripts
import qualified PlutusTx

import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           (CurrencySymbol,AssetClass,TokenName (..), Value,assetClass,assetClassValue)
import qualified Ledger.Value           as Value

import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Semigroup         (Last (..))
import           GHC.Generics           (Generic)
import qualified PlutusTx.AssocMap      as AssocMap
import           Prelude                (Semigroup (..))
import qualified Prelude                as Haskell
import           Schema                 (ToSchema)

{- HLINT ignore "Use uncurry" -}

clapAssetClass :: CurrencySymbol  -> AssetClass
clapAssetClass clapPolicyHash = assetClass clapPolicyHash (TokenName "CLAP")

clapTotalSupply :: CurrencySymbol -> Value
clapTotalSupply clapPolicyHash
    = assetClassValue
        (clapAssetClass clapPolicyHash )
        1000000000

clapMintingPolicyValidator :: TxOutRef -> () -> V.ScriptContext -> Bool
clapMintingPolicyValidator (TxOutRef refHash refIdx) _ ctx@V.ScriptContext{V.scriptContextTxInfo=txinfo} =
    let
        -- see note [Obtaining the currency symbol]
        clapPolicyHash = V.ownCurrencySymbol ctx

        minted = V.txInfoMint txinfo
        expected = clapTotalSupply clapPolicyHash

        -- True if the pending transaction mints the amount of
        -- currency that we expect
        mintOK =
            let v = expected == minted
            in traceIfFalse "C0" {-"Value minted different from expected"-} v

        -- True if the pending transaction spends the output
        -- identified by @(refHash, refIdx)@
        txOutputSpent =
            let v = V.spendsOutput txinfo refHash refIdx
            in  traceIfFalse "C1" {-"Pending transaction does not spend the designated transaction output"-} v

    in mintOK && txOutputSpent

oneShotMintingPolicy :: TxOutRef -> MintingPolicy
oneShotMintingPolicy txOutRef = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . clapMintingPolicyValidator ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode txOutRef



newtype CurrencyError =
    CurContractError ContractError
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CurrencyError

instance AsContractError CurrencyError where
    _ContractError = _CurContractError

mintCLAPContract
    :: forall w s e.
    ( AsCurrencyError e
    )
    => PubKeyHash
    -> Contract w s e CurrencySymbol
mintCLAPContract pk =
    mapError (review _CurrencyError) $ do
    (currentClapPolicyHash, currentClapMintingPolicy , txOutRef )
        <- (\txOutRef -> ( (scriptCurrencySymbol . oneShotMintingPolicy) txOutRef
                            , oneShotMintingPolicy txOutRef
                            , txOutRef  )) <$> getUnspentOutput
    utxo <- utxoAt (pubKeyHashAddress pk) -- TODO: use chain index
    tx <- submitTxConstraintsWith
            @Scripts.Any
            (Constraints.mintingPolicy currentClapMintingPolicy <> Constraints.unspentOutputs utxo)
            (Constraints.mustSpendPubKeyOutput txOutRef <> Constraints.mustMintValue (clapTotalSupply currentClapPolicyHash))
    _ <- awaitTxConfirmed (txId tx)
    pure currentClapPolicyHash

type CurrencySchema = Endpoint "Mint CLAPs" ()

mintCLAPs :: Promise (Maybe (Last CurrencySymbol)) CurrencySchema CurrencyError CurrencySymbol
mintCLAPs = endpoint @"Mint CLAPs" $ \() -> do
    ownPK <- pubKeyHash <$> ownPubKey
    cur <- mintCLAPContract ownPK
    tell (Just (Last cur))
    pure cur
