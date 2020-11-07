{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
import Clash.Prelude hiding (rom)
import Clash.Annotations.TH

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.ACIA
import Hardware.Video

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Clock
import RetroClash.Port
import RetroClash.Memory
import RetroClash.SerialRx
import RetroClash.VGA

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "RX"        ::: Signal Dom25 Bit
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board rx = vga
      where
        (frameEnd, vga) = video vidWrite
        (vidReady, vidWrite) = screenEditor outByte

        cpuOut@CPUOut{..} = mealyCPU (initState 0x0000) defaultOut cpu CPUIn{..}

        interruptRequest = pure False

        inByte = fmap unpack <$> serialRx @8 (SNat @9600) rx

        (dataIn, (outByte, ())) = memoryMap _addrOut _dataOut $ ports <||> mem
          where
            ports = do
                tx <- mask 0xde $ port $ acia inByte vidReady
                return tx

            mem = do
                mask @15 0x0000 $ readOnly $ fmap unpack . romFilePow2 "_build/intel8080/image.bin"
                mask @15 0x8000 $ readWrite $ blockRamU ClearOnReset (SNat @0x8000) (const 0)

makeTopEntity 'topEntity
