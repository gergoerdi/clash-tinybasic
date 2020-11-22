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
type TextSize = TextWidth * TextHeight
type TextAddr = Index TextSize
type TextCoord = (Index TextWidth, Index TextHeight)

type FontWidth = 8
type FontHeight = 8

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

    visible = fromSignal $ isJust <$> charX .&&. isJust <$> charY

    (newLine, lineAddr) = addressBy (snatToNum (SNat @TextWidth)) charY
    (newChar, charOffset) = addressBy 1 charX

    charAddr = lineAddr + charOffset
    charWrite = delayI Nothing w
    charLoad = delayedRam (blockRam1 ClearOnReset (SNat @TextSize) 0) charAddr charWrite

    glyphLoad = fontRom charLoad (fromMaybe 0 <$> delayI Nothing (fromSignal glyphY))
    newCol = fromSignal $ changed Nothing glyphX
    glyphRow = delayedRegister 0x00 $ \glyphRow ->
      mux (delayI False newChar) glyphLoad $
      mux (delayI False newCol) ((`shiftL` 1) <$> glyphRow) $
      glyphRow

    pixel = enable (delayI False visible) $ msb <$> glyphRow

    cursorOn = fromSignal $ riseEveryWhen (SNat @30) frameEnd
    isCursor = cursorOn .&&. charXY .==. (Just <$> cursor)

    pixel' = mux (delayI False isCursor) (fmap complement <$> pixel) pixel
    rgb = maybe frame palette <$> pixel'

    frame = (0x30, 0x30, 0x30)
    palette 0 = (0x00, 0x00, 0x00)
    palette 1 = (0x33, 0xff, 0x33)

fontRom
    :: (HiddenClockResetEnable dom)
    => DSignal dom n (Unsigned 8)
    -> DSignal dom n (Index FontHeight)
    -> DSignal dom (n + 1) (Unsigned FontWidth)
fontRom char row = delayedRom (fmap unpack . romFilePow2 "font.bin") $ toAddr <$> char <*> row
  where
    toAddr :: Unsigned 8 -> Index 8 -> Unsigned (8 + CLog 2 FontHeight)
    toAddr char row = bitCoerce (char, row)

addressBy
    :: (HiddenClockResetEnable dom, NFDataX coord, NFDataX addr, Num coord, Eq coord, Num addr)
    => addr
    -> Signal dom (Maybe coord)
    -> (DSignal dom 0 Bool, DSignal dom 1 addr)
addressBy stride coord = (new, addr)
  where
    start = fromSignal $ coord .== Just 0
    new = fromSignal $ changed Nothing coord
    addr = delayedRegister 0 $ \addr ->
        mux (delayI False start) 0 $
        mux new (addr + pure stride) $
        addr
