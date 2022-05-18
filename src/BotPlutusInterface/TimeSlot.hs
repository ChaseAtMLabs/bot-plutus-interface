module BotPlutusInterface.TimeSlot (
  TimeSlotConversionError,
  slotToPOSIXTimeImpl,
  posixTimeToSlotImpl,
  posixTimeRangeToContainedSlotRangeImpl,
) where

import Cardano.Ledger.Alonzo.TxInfo (slotToPOSIXTime)
import Ledger qualified
import Prelude

import BotPlutusInterface.QueryNode (NodeInfo (NodeInfo), queryEraHistory, querySystemStart)
import BotPlutusInterface.Types (PABConfig, pcNetwork, pcProtocolParams)
import Cardano.Api qualified as CAPI
import Cardano.Ledger.Alonzo.PParams (_protocolVersion)
import Cardano.Ledger.Slot (EpochInfo)
import Cardano.Slotting.EpochInfo (hoistEpochInfo)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, newEitherT, runEitherT)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as Text
import Ouroboros.Consensus.HardFork.History qualified as Consensus
import System.Environment (getEnv)

import Cardano.Slotting.Time (RelativeTime, toRelativeTime, SlotLength (getSlotLength), slotLengthToSec, slotLengthToMillisec)
import Data.Time (secondsToNominalDiffTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Ouroboros.Consensus.HardFork.History.Qry qualified as HF
import Ledger (Interval(Interval), Extended (Finite, NegInf, PosInf), LowerBound (LowerBound), UpperBound (UpperBound), Closure)
import Debug.Trace (traceM)
import Data.Fixed (resolution)

data TimeSlotConversionError
  = TimeSlotConversionError !Text
  deriving stock (Show)

slotToPOSIXTimeImpl :: PABConfig -> Ledger.Slot -> IO (Either TimeSlotConversionError Ledger.POSIXTime)
slotToPOSIXTimeImpl pabConf (Ledger.Slot s) = runEitherT $ do
  let pparams =
        CAPI.toLedgerPParams
          CAPI.ShelleyBasedEraAlonzo -- TODO: should era be passed as an argument?
          (pcProtocolParams pabConf)

  sock <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
  let nodeInfo = NodeInfo (pcNetwork pabConf) sock

  eraHsitory <- newET (queryEraHistory nodeInfo)
  
  sysStart <- newET $ querySystemStart nodeInfo

  let slotNo = CAPI.SlotNo $ fromInteger s
      epochInfo = toLedgerEpochInfo eraHsitory

  (relativeTime, slotLength) <- 
      firstEitherT toError $
        hoistEither $ CAPI.getProgress slotNo eraHsitory

  traceM $ "Slot length: " ++ show (Ledger.POSIXTime $ slotLengthToMillisec slotLength)

  firstEitherT toError $
    hoistEither $
      slotToPOSIXTime pparams epochInfo sysStart slotNo

toLedgerEpochInfo ::
  CAPI.EraHistory mode ->
  EpochInfo (Either CAPI.TransactionValidityError)
toLedgerEpochInfo (CAPI.EraHistory _ interpreter) =
  hoistEpochInfo (first CAPI.TransactionValidityIntervalError . runExcept) $
    Consensus.interpreterToEpochInfo interpreter

posixTimeToSlotImpl :: PABConfig -> Ledger.POSIXTime -> IO (Either TimeSlotConversionError Ledger.Slot)
posixTimeToSlotImpl pabConf pTime = runEitherT $ do
  sock <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
  let nodeInfo = NodeInfo (pcNetwork pabConf) sock

  (CAPI.EraHistory _ interpreter) <- newET (queryEraHistory nodeInfo)
  sysStart <- newET $ querySystemStart nodeInfo

  let time :: RelativeTime = toRelativeTime sysStart (toUtc pTime)
      timeQuery = HF.wallclockToSlot time
      int = HF.interpretQuery interpreter timeQuery
  (CAPI.SlotNo s, _, _) <- firstEitherT toError $ hoistEither int

  return $ Ledger.Slot (toInteger s)
  where
    toUtc (Ledger.POSIXTime milliseconds) =
      posixSecondsToUTCTime $
        secondsToNominalDiffTime
          (fromInteger $ milliseconds `div` 1000) -- FIXME: is it safe?

newET :: Show e => IO (Either e a) -> EitherT TimeSlotConversionError IO a
newET = firstEitherT toError . newEitherT

toError :: Show e => e -> TimeSlotConversionError
toError = TimeSlotConversionError . Text.pack . show

posixTimeRangeToContainedSlotRangeImpl ::
  PABConfig  ->
  Ledger.POSIXTimeRange ->
  IO (Either TimeSlotConversionError Ledger.SlotRange)
posixTimeRangeToContainedSlotRangeImpl 
  pabConf 
  ptr@(Interval (LowerBound start startIncl) (UpperBound end endIncl)) = runEitherT $ do

  startSlot <- newET $ convertExtended start
  startSlotClosure <- getClosure startSlot startIncl

  endSlot <- newET $ convertExtended end
  endSlotClosure <- getClosure endSlot endIncl


  let lowerB = LowerBound startSlot startSlotClosure
      upperB = UpperBound endSlot endSlotClosure
  
  pure $ Interval lowerB upperB
  where
    toSlot pTime = 
      posixTimeToSlotImpl pabConf pTime

    convertExtended :: Extended Ledger.POSIXTime -> IO (Either TimeSlotConversionError (Extended Ledger.Slot))
    convertExtended = \case
      Finite pTime -> fmap Finite <$> toSlot pTime
      NegInf -> pure $ Right NegInf
      PosInf -> pure $ Right PosInf

    getClosure :: Extended Ledger.Slot -> Closure -> EitherT TimeSlotConversionError IO Bool
    getClosure exSlot currentClosure = case exSlot of
      Finite slot -> do 
          slotsTime <- newEitherT $ slotToPOSIXTimeImpl pabConf slot
          pure $ slotsTime `Ledger.member` ptr

      NegInf -> pure currentClosure
      PosInf -> pure currentClosure
        

-- posixTimeRangeToContainedSlotRange :: SlotConfig -> POSIXTimeRange -> SlotRange
-- posixTimeRangeToContainedSlotRange sc ptr = case fmap (posixTimeToEnclosingSlot sc) ptr of
--   Interval (LowerBound start startIncl) (UpperBound end endIncl) ->
--     Interval
--       (LowerBound start (case start of Finite s -> slotToBeginPOSIXTime sc s `member` ptr; _ -> startIncl))
--       (UpperBound end (case end of Finite e -> slotToEndPOSIXTime sc e `member` ptr; _ -> endIncl))