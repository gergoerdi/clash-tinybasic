{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Hardware.TinyBASIC.Video where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed
import RetroClash.Clock

import Data.Maybe
import Control.Monad.State

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

-- TODO: make these parameters
type TextWidth = 72
type TextHeight = 50
type FontWidth = 8
type FontHeight = 8
type ScreenWidth = TextWidth * FontWidth
type ScreenHeight = TextHeight * FontHeight
type TextAddr = Index (TextHeight * TextWidth)
type TextCoord = (Index TextWidth, Index TextHeight)

data EditorState
    = Ready (Index TextWidth) (Index TextHeight) TextAddr
    | Clear (Index TextWidth) (Index TextHeight) TextAddr
    deriving (Generic, NFDataX)

screenEditor
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> (Signal dom Bool, Signal dom TextCoord, Signal dom (Maybe (TextAddr, Unsigned 8)))
screenEditor = mealyStateB step (Ready 0 0 0)
  where
    addr base x = base + fromIntegral x

    step chr = do
        (ready, write) <- putChar chr
        cursor <- gets $ \case
            Clear x y base -> (x, y)
            Ready x y base -> (x, y)
        return (ready, cursor, write)

    putChar chr = get >>= \case
        Clear x y base -> do
            put $ maybe (Ready 0) Clear (succIdx x) y base
            return (False, Just (addr base x, 0x20))
        Ready x y base -> case chr of
            Nothing -> do
                return (True, Nothing)
            Just 0x0a -> do
                put $ Clear 0 (nextIdx y) (satAdd SatWrap (snatToNum (SNat @TextWidth)) base)
                return (False, Nothing)
            Just chr -> do
                ready <- case succIdx x of
                    Just x' -> do
                        put $ Ready x' y base
                        return True
                    Nothing -> do
                        put $ Clear 0 (nextIdx y) (satAdd SatWrap (snatToNum (SNat @TextWidth)) base)
                        return False
                return (ready, Just (addr base x, chr))

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 TextCoord
    -> Signal Dom25 (Maybe (TextAddr, Unsigned 8))
    -> ( Signal Dom25 Bool
       , VGAOut Dom25 8 8 8
       )
video (fromSignal -> cursor) (fromSignal -> w) = (frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    (charX, glyphX) = scale @TextWidth (SNat @FontWidth) . center $ vgaX
    (charY, glyphY) = scale @TextHeight (SNat @FontHeight) . center $ vgaY
    charXY = fromSignal $ liftA2 (,) <$> charX <*> charY

    frame = pure (0x30, 0x30, 0x30)
    fg = pure maxBound
    bg = pure minBound

    isFrame = isNothing <$> charXY
    cursorOn = fromSignal $ riseEveryWhen (SNat @30) frameEnd
    isCursor = cursorOn .&&. charXY .==. (Just <$> cursor)

    rgb =
        mux (delayI False isFrame) frame $
        mux (delayI False isCursor) (mux pixel bg fg) $
        mux pixel fg bg

    pixel = bitToBool . msb <$> glyphRow

    newLine = fromSignal $ changed Nothing charY
    lineAddr = delayedRegister 0 $ \lineAddr ->
        mux (delayI False . fromSignal $ charY .==. pure (Just 0)) 0 $
        mux newLine (lineAddr + snatToNum (SNat @TextWidth)) $
        lineAddr

    newChar = fromSignal $ changed Nothing charX
    charAddr = delayedRegister 0 $ \charAddr ->
        mux (delayI False . fromSignal $ charX .==. pure (Just 0)) lineAddr $
        mux (delayI False newChar) (charAddr + 1) $
        charAddr

    charWrite = delayI Nothing w
    charLoad = delayedRam (blockRam1 ClearOnReset (SNat @(TextWidth * TextHeight)) 0) charAddr charWrite

    glyphAddr = (,) <$> charLoad <*> (fromMaybe 0 <$> delayI Nothing (fromSignal glyphY))
    glyphLoad = fontRom glyphAddr
    glyphRow = delayedRegister 0x00 $ \glyphRow ->
      mux (delayI False newChar) glyphLoad $
      (`shiftL` 1) <$> glyphRow

fontRom :: (HiddenClockResetEnable dom) => DSignal dom n (Unsigned 8, Index 8) -> DSignal dom (n + 1) (Unsigned 8)
fontRom = delayedRom $ fmap unpack . romFilePow2 "font.bin" . fmap toAddr
  where
    toAddr :: (Unsigned 8, Index 8) -> Unsigned (8 + 3)
    toAddr = bitCoerce
