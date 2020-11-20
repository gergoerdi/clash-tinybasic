{-# LANGUAGE ApplicativeDo #-}
module Hardware.TinyBASIC.Keyboard where

import Clash.Prelude

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.PS2
import RetroClash.PS2.ASCII

import Control.Monad
import Control.Lens

keyboard
    :: (HiddenClockResetEnable dom, KnownNat (ClockDivider dom (Microseconds 1)))
    => PS2 dom -> Signal dom (Maybe (Unsigned 8))
keyboard ps2 = fmap extend <$> decoded
  where
    inScanCode = parseScanCode . decodePS2 . samplePS2 $ ps2
    modState = mooreState updateMods id (False, False) inScanCode
      where
        updateMods sc = forM_ (modEvent =<< sc) $ \(ev, mod) -> do
            let newState = case ev of
                    KeyPress -> True
                    KeyRelease -> False
            case mod of
                Shift -> _1 .= newState
                Ctrl -> _2 .= newState
                _ -> return ()

        modEvent (ScanCode ev kc) = do
            (mod, side) <- modMap kc
            return (ev, mod)

    rawChar = ((asciiMap <=< keyPress) =<<) <$> inScanCode
    decoded = do
        ~(shift, ctrl) <- modState
        c <- rawChar
        pure $ case (ctrl, c) of
            (True, Just 0x63) -> Just 0x03 -- Ctrl-C
            (False, Just c) -> Just $ shiftASCII shift c
            _ -> Nothing

shiftASCII :: Bool -> Unsigned 7 -> Unsigned 7
shiftASCII shift c
  | c > 0x40 = if shift then c `setBit` 5 else c `clearBit` 5
  | c > 0x20 = if shift then c `clearBit` 4 else c
  | otherwise = c
