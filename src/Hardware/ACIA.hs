module Hardware.ACIA where

import Clash.Prelude

import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx
import RetroClash.Clock
import RetroClash.Port

import Data.Maybe
import Control.Monad.State

-- Asynchronous Communications Interface Adapter
acia
    :: (KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom Bit
    -> Signal dom (Maybe (PortCommand (Unsigned 1) (Unsigned 8)))
    -> (Signal dom (Maybe (Unsigned 8)), Signal dom Bit)
acia rate rx cmd = (portOut, tx)
  where
    inByte = fmap unpack <$> serialRx @8 rate rx
    outFifo = fifo (fmap pack <$> outByte) txDone
    (tx, txDone) = serialTx @8 rate outFifo

    (portOut, outByte) = mealyStateB step Nothing (inByte, isNothing <$> outFifo, cmd)

    step (inByte, outReady, cmd) = do
        traverse (put . Just) inByte
        case cmd of
            Just (ReadPort 0x0) -> do
                inReady <- isJust <$> get
                let val = (if inReady then 0x01 else 0x00) .|.
                          (if outReady then 0x02 else 0x00)
                return (Just val, Nothing)
            Just (WritePort 0x0 x) -> do
                return (Just 0x00, Nothing)

            Just (ReadPort 0x1) -> do
                queued <- get <* put Nothing
                return (Just $ fromMaybe 0x00 queued, Nothing)
            Just (WritePort 0x1 x) -> do
                return (Just 0x00, Just x)

            _ -> return (Nothing, Nothing)
