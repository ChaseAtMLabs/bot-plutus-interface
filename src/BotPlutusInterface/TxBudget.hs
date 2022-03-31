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
import Ledger (ExBudget (ExBudget), ExCPU (ExCPU), ExMemory (ExMemory))
import Control.Applicative ((<|>))

-- import BotPlutusInterface.Files (txFilePath)

-- data EstimateType = FromFile {txPath :: FilePath}

data BudgetEstimationError = BudgetEstimationError Text
  deriving stock (Show)


data TxFile
  = Raw FilePath
  | Signed FilePath



estimateBudgetFile :: TxFile -> IO (Either BudgetEstimationError TxBudget)
estimateBudgetFile txFile = do
  sock <- getEnv "CARDANO_NODE_SOCKET_PATH"
  let debugNodeInf = NodeInfo CAPI.Mainnet sock
  txBody <- case txFile of
              Raw rp -> deserialiseRaw rp
              Signed sp -> fmap CAPI.getTxBody <$> deserialiseSigned sp

  budgetRes <-
    either
      (pure . Left)
      (getExUnits debugNodeInf)
      txBody
  return (toTxBudget <$> budgetRes)

deserialiseSigned :: FilePath -> IO (Either BudgetEstimationError (CAPI.Tx CAPI.AlonzoEra))
deserialiseSigned txFile = do
  envlp <- readEnvelope
  return $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
      left toBudgetError
        . CAPI.deserialiseFromTextEnvelope CAPI.AsAlonzoTx


deserialiseRaw :: FilePath -> IO (Either BudgetEstimationError (CAPI.TxBody CAPI.AlonzoEra))
deserialiseRaw txFile = do
  envlp <- readEnvelope
  return $ envlp >>= parseTx
  where
    readEnvelope =
      left toBudgetError
        <$> CAPI.readTextEnvelopeFromFile txFile

    parseTx =
     (left toBudgetError
        . CAPI.deserialiseFromTextEnvelope (CAPI.AsTxBody CAPI.AsAlonzoEra ))


type ApiUnitsMap = Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError CAPI.ExecutionUnits)
type ExBudgetsMap = Map CAPI.ScriptWitnessIndex (Either CAPI.ScriptExecutionError ExBudget )

data TxBudget = TxBudget
  { exUnitsMap :: ApiUnitsMap,
    exBudgetsMap :: ExBudgetsMap,
    overallMax :: Either CAPI.ScriptExecutionError ExBudget,
    overallSum :: Either CAPI.ScriptExecutionError ExBudget
  }
  deriving stock (Show)

toTxBudget :: ApiUnitsMap -> TxBudget
toTxBudget bdg =
  TxBudget bdg exBudgets overallMax' overallSum'
  where
    exBudgets =  fmap unitsToBudget <$> bdg

    eitherBudgets :: Either CAPI.ScriptExecutionError [ExBudget]
    eitherBudgets = sequence $ Map.elems exBudgets

    overallSum' = mconcat <$> eitherBudgets
    overallMax' = foldl' takeMax mempty <$> eitherBudgets

takeMax :: ExBudget -> ExBudget -> ExBudget
takeMax (ExBudget s1 m1) (ExBudget s2 m2) =
  ExBudget (max s1 s2) (max m1 m2)

unitsToBudget (CAPI.ExecutionUnits cpu mem) =
    ExBudget (ExCPU $ cast cpu)  (ExMemory $ cast mem)
  where
    cast = fromInteger . toInteger

getExUnits ::
  NodeInfo ->
  CAPI.TxBody CAPI.AlonzoEra ->
  IO (Either BudgetEstimationError ApiUnitsMap)
getExUnits nodeInf txBody = do
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