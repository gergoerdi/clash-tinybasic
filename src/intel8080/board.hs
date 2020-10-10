{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE CPP #-}

import Clash.Prelude hiding (rom)
import Clash.Annotations.TH

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.ACIA

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Clock
import RetroClash.Port
import RetroClash.Memory

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

createDomain vSystem{vName="Native", vPeriod = hzToPeriod __NATIVE_CLOCK__}

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

        interruptRequest = pure False

        (dataIn, (tx, ())) = memoryMap (Just <$> _addrOut) _dataOut $ ports <||> mem
          where
            ports = do
                tx <- mask 0xde $ port $ acia (SNat @9600) rx
                return tx

            mem = do
                mask @15 0x0000 $ rom $ fmap unpack . romFilePow2 "_build/intel8080/image.bin"
                mask @15 0x8000 $ ram $ blockRamU ClearOnReset (SNat @0x8000) (const 0)

makeTopEntity 'topEntity
