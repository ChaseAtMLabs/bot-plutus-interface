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

main :: IO ()
main = do
  -- TODO: export PATH=$PATH:/home/mike/dev/mlabs/local-cluster/node-bins 
  let clusterDir = "/home/mike/dev/mlabs/local-cluster/data"
  [sockPath] <- getArgs
  setEnv "CARDANO_NODE_SOCKET_PATH" sockPath
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
  pparams :: ProtocolParameters <- getOrFail <$> BPI.queryProtocolParams nodeInfo
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
    , pcOwnPubKeyHash = "a696c591f6d77c164262aa93821483713575375c39c5ca14f3a493d8" -- FIXME: parsing key hash
    , pcScriptFileDir = Text.pack$ clusterDir </> "bot-plutus-interface/scripts"
    , pcSigningKeyFileDir = Text.pack$ clusterDir </> "bot-plutus-interface/signing-keys"
    , pcTxFileDir = Text.pack$ clusterDir </> "bot-plutus-interface/txs"
    , pcDryRun = False
    , pcLogLevel = Debug
    , pcProtocolParamsFile = pparamsFile
    , pcForceBudget = Just (1000, 1000)
    , pcEnableTxEndpoint = True
    }

getPkhs bpiDir = do
  let dir = bpiDir </> "bot-plutus-interface/signing-keys"
      replace = PubKeyHash
                . stringToBuiltinByteString
                . Text.unpack
                . Text.replace "signing-key-" ""
                . Text.replace ".skey" ""
                . Text.pack
  keyNames <- listDirectory dir
  return $ map replace keyNames

getOrFail = either (error . show) id