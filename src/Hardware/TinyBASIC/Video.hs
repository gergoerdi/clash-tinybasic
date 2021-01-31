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
    = Ready (Index TextHeight) TextAddr (Index TextWidth)
    | Clear (Index TextHeight) TextAddr (Index TextWidth)
    deriving (Generic, NFDataX)

screenEditor
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom (Maybe (TextAddr, Unsigned 8))
       , Signal dom TextCoord
       , Signal dom Bool
       )
screenEditor = mealyStateB step (Ready 0 0 0)
  where
    base `offsetBy` x = base + fromIntegral x
    stride = snatToNum (SNat @TextWidth)
    nextLine = satAdd SatWrap stride

    step chr = do
        write <- putChar chr
        (cursor, ready) <- gets $ \case
            Clear y base x -> ((x, y), False)
            Ready y base x -> ((x, y), True)
        return (write, cursor, ready)

    putChar chr = get >>= \case
        Clear y base x -> do
            put $ maybe (Ready y base 0) (Clear y base) $ succIdx x
            return $ Just (base `offsetBy` x, 0x20)
        Ready y base x -> case chr of
            Nothing -> do
                return Nothing
            Just 0x0a -> do
                put $ Clear (nextIdx y) (nextLine base) 0
                return Nothing
            Just chr -> do
                put $ maybe (Clear (nextIdx y) (nextLine base) 0) (Ready y base) $ succIdx x
                return $ Just (base `offsetBy` x, chr)

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
    visible = isJust <$> charXY

    (newLine, lineAddr) = addressBy (snatToNum (SNat @TextWidth)) charY
    (newChar, charOffset) = addressBy 1 charX

    charAddr = lineAddr + charOffset
    charWrite = delayI Nothing w
    charLoad = delayedRam (blockRam1 ClearOnReset (SNat @TextSize) 0) charAddr charWrite

    glyphLoad = enable (delayI False newChar) $ fontRom charLoad (fromMaybe 0 <$> delayI Nothing (fromSignal glyphY))
    newCol = delayI False $ fromSignal $ changed Nothing glyphX
    pixel = enable (delayI False visible) $ liftD2 shifterL glyphLoad newCol

    cursorOn = fromSignal $ oscillateWhen False $ riseEveryWhen (SNat @30) frameEnd
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
fontRom char row = delayedRom (fmap unpack . romFilePow2 "font.bin") $
    toAddr <$> char <*> row
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
