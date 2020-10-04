{-# LANGUAGE RecordWildCards, ApplicativeDo, NumericUnderscores #-}
{-# LANGUAGE DeriveFunctor, LambdaCase #-}
{-# LANGUAGE CPP #-}

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Intel8080
import Hardware.Intel8080.CPU

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.SerialRx
import RetroClash.SerialTx
import RetroClash.Clock
import RetroClash.Port

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

createDomain vSystem{vName="Native", vPeriod = hzToPeriod __NATIVE_CLOCK__}

serialIO
    :: (KnownNat (ClockDivider dom (HzToPeriod rate)), HiddenClockResetEnable dom)
    => SNat rate
    -> Signal dom Bit
    -> Signal dom (Maybe (PortCommand (Unsigned 1) Value))
    -> (Signal dom Value, Signal dom Bit)
serialIO rate rx cmd = (portOut, tx)
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
                return (val, Nothing)
            Just (WritePort 0x0 x) -> do
                return (0x00, Nothing)

            Just (ReadPort 0x1) -> do
                queued <- get <* put Nothing
                return (fromMaybe 0x00 queued, Nothing)
            Just (WritePort 0x1 x) -> do
                return (0x00, Just x)

            _ -> return (0x00, Nothing)

topEntity
    :: "CLK"   ::: Clock Native
    -> "RESET" ::: Reset Native
    -> "RX"    ::: Signal Native Bit
    -> "TX"    ::: Signal Native Bit
topEntity = withEnableGen board
  where
    board rx = tx
      where
        cpuOut@CPUOut{..} = mealyCPU initState defaultOut (void . runMaybeT . cpu) CPUIn{..}

        memAddr = either (const 0) id <$> _addrOut
        memWrite = do
            addr <- _addrOut
            write <- _dataOut
            pure $ case (addr, write) of
                (Right addr, Just write) -> Just (addr, pack write)
                _ -> Nothing

        portCmd = do
            addr <- _addrOut
            write <- _dataOut
            pure $ case addr of
                Left port -> Just $ maybe (ReadPort port) (WritePort port) write
                Right addr -> Nothing

        memData = unpack <$> blockRamFile (SNat @0x10000) "image.bin" memAddr memWrite

        dataIn = muxA
            [ delay Nothing portIn
            , Just <$> memData
            ]
        interruptRequest = pure False

        serCmd = do
            cmd <- portCmd
            pure $ bitraverse (basedAt 0xde) pure =<< cmd

        (serIn, tx) = serialIO (SNat @9600) rx serCmd
        portIn = muxA
            [ enable (isJust <$> serCmd) serIn
            ]

makeTopEntity 'topEntity
