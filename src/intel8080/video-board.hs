{-# LANGUAGE RecordWildCards, NumericUnderscores, LambdaCase #-}
import Clash.Prelude
import Clash.Annotations.TH

import Hardware.TinyBASIC.Intel8080
import Hardware.TinyBASIC.Keyboard
import Hardware.TinyBASIC.Video

import RetroClash.Utils
import RetroClash.VGA
import RetroClash.PS2

import Control.Monad
import Control.Lens

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "PS2"       ::: PS2 Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board ps2 = vga
      where
        (frameEnd, vga) = video cursor vidWrite
        (vidReady, cursor, vidWrite) = screenEditor outByte

        outByte = logicBoard inByte vidReady
        inByte = keyboard ps2

makeTopEntity 'topEntity
