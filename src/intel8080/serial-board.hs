{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
{-# LANGUAGE CPP #-}

import Clash.Prelude hiding (rom)
import Clash.Annotations.TH

import Hardware.TinyBASIC.Intel8080

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.SerialRx
import RetroClash.SerialTx

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
        outByte = logicBoard inByte outReady

        inByte = fmap unpack <$> serialRx (SNat @9600) rx
        (tx, outReady) = serialTx (SNat @9600) (fmap pack <$> outByte)

makeTopEntity 'topEntity
