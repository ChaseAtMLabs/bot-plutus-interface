{-# LANGUAGE ViewPatterns #-}

module BotPlutusInterface.QueryNode where

import Prelude

import Cardano.Api qualified as C
import Cardano.Api.ProtocolParameters (ProtocolParameters)
-- import Cardano.Launcher.Node (nodeSocketFile)
-- import Cardano.Slotting.Slot (WithOrigin)
-- import Cardano.Slotting.Time (SystemStart)
-- import Cardano.Wallet.Shelley.Launch.Cluster (RunningNode (RunningNode))
-- import Control.Exception (Exception)
-- import Data.Set qualified as Set
-- import GHC.Generics (Generic)
-- import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
-- import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
-- import Test.Plutip.Internal.Types (ClusterEnv (runningNode))

data NodeQueryError
  = QueryError String
  deriving stock (Eq, Show)

data NodeInfo = NodeInfo
  { niNetworkId :: C.NetworkId
  , niSocket :: FilePath
  }
-- instance Exception CardanoApiError

-- | Get current block using `Cardano.Api` library
-- currentBlock :: NodeInfo -> IO (Either NodeQueryError (WithOrigin C.BlockNo))
-- currentBlock (connectionInfo -> cInfo) = 
  -- C.queryNodeLocalState cInfo Nothing C.QueryChainBlockNo

queryProtocolParams :: NodeInfo -> IO (Either NodeQueryError ProtocolParameters)
queryProtocolParams (connectionInfo -> cInfo) =
  flattenQueryResult <$> C.queryNodeLocalState cInfo Nothing query
  where
    query = C.QueryInEra C.AlonzoEraInCardanoMode 
             $ C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo C.QueryProtocolParameters

flattenQueryResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either NodeQueryError b
flattenQueryResult = \case
  Right (Right res) -> Right res
  err -> Left $ QueryError (show err)

connectionInfo :: NodeInfo -> C.LocalNodeConnectInfo C.CardanoMode
connectionInfo (NodeInfo netId socket) =
  C.LocalNodeConnectInfo
    (C.CardanoModeParams (C.EpochSlots 21600)) -- TODO: is this number correct?
    netId
    socket

-- utxosAtAddress :: ClusterEnv -> C.AddressAny -> IO (Either CardanoApiError (C.UTxO C.AlonzoEra))
-- utxosAtAddress (runningNode -> rn) addr = do
--   flattenQueryResult <$> C.queryNodeLocalState info Nothing query
--   where
--     info = connectionInfo rn
--     query =
--       shellyBasedAlonzoQuery
--         (C.QueryUTxO $ C.QueryUTxOByAddress (Set.singleton addr))

-- queryProtocolParams :: ClusterEnv -> IO (Either CardanoApiError ProtocolParameters)
-- queryProtocolParams (runningNode -> rn) =
--   flattenQueryResult <$> C.queryNodeLocalState info Nothing query
--   where
--     info = connectionInfo rn
--     query = shellyBasedAlonzoQuery C.QueryProtocolParameters

-- netId :: C.NetworkId
-- netId = C.Mainnet

-- connectionInfo :: RunningNode -> C.LocalNodeConnectInfo C.CardanoMode
-- connectionInfo (RunningNode socket _ _) =
--   C.LocalNodeConnectInfo
--     (C.CardanoModeParams (C.EpochSlots 21600))
--     netId
--     (nodeSocketFile socket)

-- conInfo = connectionInfo . runningNode

-- queryTip :: RunningNode -> IO C.ChainTip
-- queryTip = C.getLocalChainTip . connectionInfo

-- shellyBasedAlonzoQuery ::
--   C.QueryInShelleyBasedEra C.AlonzoEra result ->
--   C.QueryInMode C.CardanoMode (Either EraMismatch result)
-- shellyBasedAlonzoQuery =
--   C.QueryInEra C.AlonzoEraInCardanoMode
--     . C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo

-- flattenQueryResult ::
--   (Show e1, Show e2, Show b) =>
--   Either e1 (Either e2 b) ->
--   Either CardanoApiError b
-- flattenQueryResult = \case
--   Right (Right res) -> Right res
--   err -> Left $ SomeError (show err)

-- -- systemStart :: ClusterEnv -> IO (Either AcquireFailure (WithOrigin C.BlockNo))
-- systemStart :: ClusterEnv -> IO (Either AcquireFailure SystemStart)
-- systemStart cEnv = do
--   C.queryNodeLocalState
--     (conInfo cEnv)
--     Nothing
--     C.QuerySystemStart

-- protocolParams :: ClusterEnv -> IO (Either CardanoApiError ProtocolParameters)
-- protocolParams cEnv =
--   flattenQueryResult
--     <$> C.queryNodeLocalState
--       (conInfo cEnv)
--       Nothing
--       (shellyBasedAlonzoQuery C.QueryProtocolParameters)

-- eraHistory :: ClusterEnv -> IO (Either AcquireFailure (C.EraHistory C.CardanoMode))
-- eraHistory cEnv =
--   C.queryNodeLocalState
--     (conInfo cEnv)
--     Nothing
--     (C.QueryEraHistory C.CardanoModeIsMultiEra)

-- outsByInputs :: ClusterEnv -> [C.TxIn] -> IO (Either CardanoApiError (C.UTxO C.AlonzoEra))
-- outsByInputs cEnv ins =
--   flattenQueryResult
--     <$> C.queryNodeLocalState
--       (conInfo cEnv)
--       Nothing
--       -- (shellyBasedAlonzoQuery (C.QueryUTxO (C.QueryUTxOByTxIn (Set.fromList ins))))
--       (shellyBasedAlonzoQuery (C.QueryUTxO C.QueryUTxOWhole))

-- queryLocalState cEnv =
--   C.queryNodeLocalState
--     (conInfo cEnv)
--     Nothing
--     . shellyBasedAlonzoQuery