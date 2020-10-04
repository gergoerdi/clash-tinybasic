{-# LANGUAGE RecordWildCards, ApplicativeDo, NumericUnderscores #-}
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

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State

createDomain vSystem{vName="Native", vPeriod = hzToPeriod __NATIVE_CLOCK__}

data PortCommand port a
    = ReadPort port
    | WritePort port a
    deriving (Generic, NFDataX, Show)

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

        memData = unpack <$> blockRamFile (SNat @0x10000) "image-i8080.bin" memAddr memWrite

        dataIn = muxA [ delay Nothing portIn, Just <$> memData ]
        interruptRequest = pure False

        inByte = bitCoerce <$> serialRx @8 (SNat @9600) rx
        outFifo = fifo (bitCoerce <$> outByte) txDone
        (tx, txDone) = serialTx @8 (SNat @9600) outFifo

        (portIn, outByte) = mealyStateB step Nothing (portCmd, inByte, isNothing <$> outFifo)
          where
            step :: (Maybe (PortCommand Port Value), Maybe Value, Bool) -> State (Maybe Value) (Maybe Value, Maybe Value)
            step (cmd, inByte, outputReady) = do
                traverse (put . Just) inByte
                case cmd of
                    Just (ReadPort 0xde) -> do
                        inputReady <- isJust <$> get
                        let val = (if inputReady then 0x01 else 0x00) .|.
                                  (if outputReady then 0x02 else 0x00)
                        return (Just val, Nothing)
                    Just (WritePort 0xde x) -> do
                        return (Just 0x00, Nothing)

                    Just (ReadPort 0xdf) -> do
                        queued <- get <* put Nothing
                        return (Just $ fromMaybe 0x00 queued, Nothing)
                    Just (WritePort 0xdf x) -> do
                        return (Just 0x00, Just x)

                    _ -> return (Nothing, Nothing)

makeTopEntity 'topEntity
