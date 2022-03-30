module BotPlutusInterface.TxBudget where

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo))
import BotPlutusInterface.QueryNode qualified as QueryNode
import Cardano.Api qualified as CAPI
import Control.Arrow (left)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory.Internal.Prelude (getEnv)
import Prelude

-- import BotPlutusInterface.Files (txFilePath)

data EstimateType = FromFile {txPath :: FilePath}

data BudgetEstimationError = BudgetEstimationError Text
  deriving stock (Show)

estimateBudgetFile :: FilePath -> IO (Either BudgetEstimationError TxBudget)
estimateBudgetFile txPath = do
  sock <- getEnv "CARDANO_NODE_SOCKET_PATH"

  let debugNodeInf = NodeInfo CAPI.Mainnet sock

  txRes <- deserialise txPath
  budgetRes <-
    either
      (pure . Left)
      (getExUnits debugNodeInf)
      txRes
  return (toTxBudget <$> budgetRes)

deserialise :: FilePath -> IO (Either BudgetEstimationError (CAPI.Tx CAPI.AlonzoEra))
deserialise txFile = do
  envlp <- readEnvelope
  return $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CAPI.deserialiseFromTextEnvelope CAPI.AsAlonzoTx

type ApiBudget = Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError CAPI.ExecutionUnits)

data TxBudget = TxBudget
  { apiMap :: ApiBudget,
    overallMax :: Either CAPI.ScriptExecutionError CAPI.ExecutionUnits
  }
  deriving stock (Show)

toTxBudget :: ApiBudget -> TxBudget
toTxBudget bdg =
  TxBudget bdg (calcOverall $ Map.elems bdg)
  where
    calcOverall els =
      foldl'
        takeMaxUnits -- TODO: Is it really has to be like that, why not sum for both scripts?
        (CAPI.ExecutionUnits 0 0)
      <$> sequence els

takeMaxUnits :: CAPI.ExecutionUnits -> CAPI.ExecutionUnits -> CAPI.ExecutionUnits
takeMaxUnits (CAPI.ExecutionUnits s1 m1) (CAPI.ExecutionUnits s2 m2) =
  CAPI.ExecutionUnits (max s1 s2) (max m1 m2)

getExUnits ::
  NodeInfo ->
  CAPI.Tx CAPI.AlonzoEra ->
  IO (Either BudgetEstimationError ApiBudget)
getExUnits nodeInf tx = do
  let txBody = CAPI.getTxBody tx
  sysStart <- QueryNode.systemStart nodeInf
  eraHist <- QueryNode.eraHistory nodeInf
  pparams <- QueryNode.protocolParams nodeInf
  utxo <- QueryNode.outsByInputs nodeInf (getIns txBody)
  return $
    flattenEvalResult $
      CAPI.evaluateTransactionExecutionUnits CAPI.AlonzoEraInCardanoMode
        <$> sysStart
        <*> eraHist
        <*> pparams
        <*> utxo
        <*> pure txBody
  where
    getIns :: CAPI.TxBody CAPI.AlonzoEra -> [CAPI.TxIn]
    getIns txBody =
      let (CAPI.TxBody txbc) = txBody
       in fst <$> CAPI.txIns txbc

toBudgetError :: Show e => e -> BudgetEstimationError
toBudgetError = BudgetEstimationError . Text.pack . show

flattenEvalResult ::
  (Show e1, Show e2, Show b) =>
  Either e1 (Either e2 b) ->
  Either BudgetEstimationError b
flattenEvalResult = \case
  Right (Right res) -> Right res
  err -> Left $ toBudgetError err