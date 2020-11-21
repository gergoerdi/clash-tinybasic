{-# LANGUAGE ApplicativeDo #-}
module Hardware.TinyBASIC.Keyboard where

import Clash.Prelude

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.PS2
import RetroClash.PS2.ASCII

keyboard
    :: (HiddenClockResetEnable dom, KnownNat (ClockDivider dom (Microseconds 1)))
    => PS2 dom -> Signal dom (Maybe (Unsigned 8))
keyboard ps2 = fmap extend <$> (toChar <$> shift <*> ctrl <*> sc)
  where
    sc = parseScanCode . decodePS2 . samplePS2 $ ps2

    shift = keyState 0x012 sc .||. keyState 0x059 sc
    ctrl = keyState 0x014 sc .||. keyState 0x114 sc

    toChar shift ctrl sc = case asciiMap =<< keyPress =<< sc of
        Just 0x63 | ctrl -> Just 0x03 -- Ctrl-C
        Just c | not ctrl -> Just $ shiftASCII shift c
        _ -> Nothing

shiftASCII :: Bool -> Unsigned 7 -> Unsigned 7
shiftASCII shift c
  | c > 0x40 = if shift then c `setBit` 5 else c `clearBit` 5
  | c > 0x20 = if shift then c `clearBit` 4 else c
  | otherwise = c
