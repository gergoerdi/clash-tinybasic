{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE RebindableSyntax #-}
module Hardware.TinyBASIC.Intel8080 (logicBoard) where

import Clash.Prelude hiding (rom)

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.ACIA

import RetroClash.CPU
import RetroClash.Port
import RetroClash.Memory2

logicBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8)) -> Signal dom Bool -> Signal dom (Maybe (Unsigned 8))
logicBoard inByte outReady = outByte
  where
    CPUOut{..} = intel8080 CPUIn{..}

    interruptRequest = pure False

    (dataIn, (), ConsR outByte NilR) = runAddressing _addrOut _dataOut $ do
        rom <- romFromFile (SNat @0x0800) "_build/intel8080/image.bin"
        ram <- ram0 (SNat @0x1800)
        acia <- port $ acia inByte outReady

        matchLeft $ do
            from 0x10 $ connect acia
        matchRight $ do
            from 0x0000 $ connect rom
            from 0x0800 $ connect ram
      where
        return = Return
        (>>=) = Bind
        (=<<) = flip (>>=)
        m >> n = Bind m (const n)
