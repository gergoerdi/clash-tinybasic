import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Intel8080
import Hardware.Intel8080.CPU

import RetroClash.Utils
import RetroClash.SerialRx
import RetroClash.SerialTx
import RetroClash.Clock

topEntity
    :: "CLOCK" ::: Clock System
    -> "RESET" ::: Reset System
    -> "RX"    ::: Signal System Bit
    -> "TX"    ::: Signal System Bit
topEntity = withEnableGen board
  where
    board rx = undefined

makeTopEntity 'topEntity
