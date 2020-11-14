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
    CPUOut{..} = intel8080 CPUIn{..}

    interruptRequest = pure False

    (dataIn, outByte) = memoryMap _addrOut _dataOut $ do
        matchRight $ do
            mask 0x0000 $ romFromFile (SNat @0x0800) "_build/intel8080/image.bin"
            mask 0x0800 $ ram0 (SNat @0x0800)
            mask 0x1000 $ ram0 (SNat @0x1000)
        matchLeft $ do
            mask 0x10 $ port $ acia inByte outReady
