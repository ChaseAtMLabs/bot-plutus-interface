{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude

import Ledger (PubKeyHash(PubKeyHash))
import BotPlutusInterface.Contract qualified as BPI
import BotPlutusInterface.QueryNode qualified as BPI
import BotPlutusInterface.Types

import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Api (NetworkId (Mainnet))
import Data.Aeson qualified as JSON
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))
import Data.Default (def)
import System.FilePath (takeExtension, (</>))
import System.Directory (listDirectory)
import Data.Text qualified as Text
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Data.UUID.V4 qualified as UUID
import Control.Concurrent.STM (newTVarIO)
import Plutus.PAB.Core.ContractInstance.STM (Activity (Active))

import LockSpend (lockThenSpend)
import System.Environment (getArgs, setEnv)
import qualified Data.Text as Text
import Wallet.Types (ContractInstanceId (ContractInstanceId))
import Cardano.Api qualified as CAPI
import Data.Aeson ((.=))
import Data.Text (Text)

main :: IO ()
main = do
  -- TODO: export PATH=$PATH:/home/mike/dev/mlabs/local-cluster/node-bins 
  let clusterDir = "/home/mike/dev/mlabs/local-cluster/data"
  [sockPath] <- getArgs
  setEnv "CARDANO_NODE_SOCKET_PATH" sockPath
  print $ JSON.encode (PubKeyHash "ffff")
  let nodeInfo = BPI.NodeInfo Mainnet sockPath

  cEnv <- mkContractEnv nodeInfo clusterDir
  BPI.runContract cEnv lockThenSpend >>= print


mkContractEnv nodeInfo clusterDir = do
  (pparams, paramsFile) <- getPparams  nodeInfo clusterDir
  contractInstanceID <- ContractInstanceId <$> UUID.nextRandom
  contractState <- newTVarIO (ContractState Active mempty)
  pkhs <- getPkhs clusterDir
  print pkhs
  return $ ContractEnvironment
              { cePABConfig = mkPabConf pparams (Text.pack paramsFile) clusterDir (head pkhs)
              , ceContractState = contractState
              , ceContractInstanceId = contractInstanceID
              }

getPparams nodeInfo clusterDir = do
  pparams :: ProtocolParameters <- getOrFailM $ BPI.queryProtocolParams nodeInfo
  let ppath = clusterDir </> "pparams.json"
  JSON.encodeFile ppath pparams
  return (pparams, ppath)


mkPabConf pparams pparamsFile clusterDir ownPkh =
  PABConfig
    { pcCliLocation = Local
    , pcNetwork = Mainnet
    , pcChainIndexUrl = BaseUrl Http "localhost" 9083 ""
    , pcPort = 9080
    , pcProtocolParams = pparams
    , pcTipPollingInterval = 1_000_000
    , pcSlotConfig = def
    , pcOwnPubKeyHash = ownPkh
    , pcScriptFileDir = Text.pack$ clusterDir </> "bot-plutus-interface/scripts"
    , pcSigningKeyFileDir = Text.pack$ clusterDir </> "bot-plutus-interface/signing-keys"
    , pcTxFileDir = Text.pack$ clusterDir </> "bot-plutus-interface/txs"
    , pcDryRun = False
    , pcLogLevel = Debug
    , pcProtocolParamsFile = pparamsFile
    , pcForceBudget = Just (1000, 1000)
    , pcEnableTxEndpoint = True
    }

getPkhs :: FilePath -> IO [PubKeyHash]
getPkhs bpiDir = do
  let dir = bpiDir </> "bot-plutus-interface/signing-keys"
      replace = Text.unpack
                . Text.replace "signing-key-" ""
                . Text.replace ".skey" ""
                . Text.pack
  keyNames <- listDirectory dir
  return $ map (parseKey . replace) keyNames
  where
    parseKey :: String -> PubKeyHash
    parseKey key = 
      let
        res = JSON.fromJSON $ JSON.object ["getPubKeyHash" .= key ]
      in case res of
        JSON.Success pkh -> pkh
        _ -> error "failed to parse pkh"

getOrFail :: Show e => Either e a -> a
getOrFail = either (error . show) id

getOrFailM :: (Show e, Functor f) => f (Either e b) -> f b
getOrFailM = (getOrFail <$>)
