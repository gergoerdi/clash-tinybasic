{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
module Hardware.TinyBASIC.Intel8080 (logicBoard) where

import Clash.Prelude hiding (rom)

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.ACIA

import RetroClash.CPU
import RetroClash.Port
import RetroClash.Memory

logicBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8)) -> Signal dom Bool -> Signal dom (Maybe (Unsigned 8))
logicBoard inByte outReady = outByte
  where
    cpuOut@CPUOut{..} = mealyCPU (initState 0x0000) defaultOut cpu CPUIn{..}

    interruptRequest = pure False

    (dataIn, (outByte, ())) = memoryMap _addrOut _dataOut $ ports <||> mem
      where
        ports = do
            outByte <- mask 0x10 $ port $ acia inByte outReady
            return outByte

        mem = do
            mask @11 0x0000 $ readOnly $ fmap unpack . romFilePow2 "_build/intel8080/image.bin"
            mask @11 0x0800 $ readWrite $ blockRamU ClearOnReset (SNat @0x0800) (const 0)
            mask @12 0x1000 $ readWrite $ blockRamU ClearOnReset (SNat @0x1000) (const 0)
