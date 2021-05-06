{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
module Hardware.TinyBASIC.Intel8080 (logicBoard) where

import Clash.Prelude hiding (rom)

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.ACIA

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Port
import RetroClash.Memory
import Data.Maybe (fromMaybe)

logicBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8)) -> Signal dom Bool -> Signal dom (Maybe (Unsigned 8))
logicBoard inByte outReady = outByte
  where
    CPUOut{..} = intel8080 CPUIn{..}

    interruptRequest = pure False

    dataIn = Just <$> (0 |>. dataIn')
    (dataIn', outByte) = $(memoryMap [|Right 0 |>. _addrOut|] [|_dataOut|] $ do
        rom <- romFromFile (SNat @0x0800) [|"_build/intel8080/image.bin"|]
        ram <- ram0 (SNat @0x1800)
        (acia, outByte) <- port @(Unsigned 1) [|acia inByte outReady|]

        matchLeft @(Unsigned 8) $ do
            from 0x10 $ connect acia
        matchRight @(Unsigned 16) $ do
            from 0x0000 $ connect rom
            from 0x0800 $ connect ram

        return outByte)
