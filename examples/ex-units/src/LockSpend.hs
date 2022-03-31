module LockSpend (lockThenSpend) where

import Prelude

import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (CardanoTx, ScriptContext (scriptContextTxInfo), TxInfo (txInfoOutputs, txInfoInputs))
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, submitTx)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)

import Ledger (Address, ScriptContext, TxId, Validator, getCardanoTxId, scriptAddress, unitDatum, unitRedeemer, validatorHash)
import Ledger.Typed.Scripts.Validators qualified as Validators
import Plutus.Contract (awaitTxConfirmed, submitTxConstraintsWith)
import Plutus.V1.Ledger.Ada qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as PP

lockThenSpend :: Contract () EmptySchema Text (TxId, CardanoTx)
lockThenSpend = do
  lockAtScript
  Contract.waitNSlots 1
  spendFromScript


lockAtScript :: Contract () EmptySchema Text (TxId, CardanoTx)
lockAtScript = do
  let constr =
        Constraints.mustPayToOtherScript
          (validatorHash validator)
          unitDatum
          (Value.adaValueOf 10)
  let constr2 =
        Constraints.mustPayToOtherScript
          (validatorHash $ validator2 2)
          unitDatum
          (Value.adaValueOf 10)
  tx <- submitTx (constr <> constr2)
  awaitTxConfirmed $ getCardanoTxId tx
  pure (getCardanoTxId tx, tx)

spendFromScript :: Contract () EmptySchema Text (TxId, CardanoTx)
spendFromScript = do
  utxos1 <- Map.toList <$> Contract.utxosAt (validatorAddr)
  utxos2 <- Map.toList <$> Contract.utxosAt (validatorAddr2 2)
  case (utxos1, utxos2) of
    ([], _) -> Contract.throwError "No UTxOs at script address"
    (_, []) -> Contract.throwError "No UTxOs at script address"
    ((oref1,_) : _, (oref2,_) : _) -> spendUtxo oref1 utxos1 oref2 utxos2
  where
    spendUtxo oref1 utxos1 oref2 utxos2 = do
      let txc1 = Constraints.mustSpendScriptOutput oref1 unitRedeemer
          lookups1 =
            Constraints.unspentOutputs (Map.fromList utxos1)
              <> Constraints.otherScript validator

      let txc2 = Constraints.mustSpendScriptOutput oref2 unitRedeemer
          lookups2 =
            Constraints.unspentOutputs (Map.fromList utxos2)
              <> Constraints.otherScript (validator2 2)

      tx <- submitTxConstraintsWith @TestLockSpend
              (lookups1 <> lookups2)
              (txc1 <> txc2)
      awaitTxConfirmed $ getCardanoTxId tx
      pure (getCardanoTxId tx, tx)


-- Always true Script and spending contract

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator  _ _ _ = True

data TestLockSpend

instance Validators.ValidatorTypes TestLockSpend where
  type DatumType TestLockSpend = ()
  type RedeemerType TestLockSpend = ()

typedValidator :: Validators.TypedValidator TestLockSpend
typedValidator =
  Validators.mkTypedValidator @TestLockSpend
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Validators.wrapValidator @() @()

validator ::  Validator
validator = Validators.validatorScript  typedValidator

validatorAddr ::  Address
validatorAddr = scriptAddress validator



{-# INLINEABLE mkValidator2 #-}
mkValidator2 :: Integer -> () -> () -> ScriptContext -> Bool
mkValidator2 i _ _ ctx =
  if i PP./= 1
    then PP.traceIfFalse "looooooooooooong" check
    else PP.traceIfFalse "short" check
  where
    check = True
  --   info = scriptContextTxInfo ctx
  --   check = PP.length (txInfoOutputs info) PP.== 1

data TestLockSpend2

instance Validators.ValidatorTypes TestLockSpend2 where
  type DatumType TestLockSpend2 = ()
  type RedeemerType TestLockSpend2 = ()

typedValidator2 :: Integer -> Validators.TypedValidator TestLockSpend
typedValidator2 uid =
  Validators.mkTypedValidator @TestLockSpend
    ($$(PlutusTx.compile [||mkValidator2||])  `PlutusTx.applyCode` PlutusTx.liftCode uid)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Validators.wrapValidator @() @()

validator2 ::  Integer -> Validator
validator2 = Validators.validatorScript . typedValidator2

validatorAddr2 ::  Integer -> Address
validatorAddr2 = scriptAddress . validator2