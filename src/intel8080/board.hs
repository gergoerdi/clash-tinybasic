{-# LANGUAGE RecordWildCards, ApplicativeDo, NumericUnderscores #-}
{-# LANGUAGE CPP #-}

import Clash.Prelude
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

        (memAddr, memWrite, portCmd) = unbundle $ do
            addr <- _addrOut
            write <- _dataOut
            pure $ case addr of
                Left port -> (Nothing, Nothing, Just $ maybe (ReadPort port) (WritePort port) write)
                Right addr -> (Just addr, write, Nothing)

        memSpec =
            UpTo    0x8000 (ROM $ fmap unpack . romFilePow2 "image.bin") $
            Default        (RAM $ blockRamU ClearOnReset (SNat @0x8000) (const 0))
        memData = memory memSpec (fromMaybe 0 <$> memAddr) memWrite

        dataIn = muxA
            [ delay Nothing portIn
            , Just <$> memData
            ]
        interruptRequest = pure False

        aciaCmd = basedAt 0xde <$> portCmd
        (aciaIn, tx) = acia (SNat @9600) rx aciaCmd

        portIn = muxA
            [ enable (isJust <$> aciaCmd) aciaIn
            ]

makeTopEntity 'topEntity
