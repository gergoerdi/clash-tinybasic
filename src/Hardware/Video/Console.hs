{-# LANGUAGE NumericUnderscores, RecordWildCards, TypeApplications #-}
{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
module Hardware.Video.Console where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed
import RetroClash.Clock
import Data.Maybe

import Control.Monad.State
import Data.Char (ord)

-- | 25 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom25", vPeriod = hzToPeriod 25_175_000}

-- TODO: make these parameters
type TextWidth = 64
type TextHeight = 48
type FontWidth = 8
type FontHeight = 8
type ScreenWidth = TextWidth * FontWidth
type ScreenHeight = TextHeight * FontHeight

type TextCoord = (Index TextHeight, Index TextWidth)

data EditorState
    = Ready (Index TextHeight) (Index TextWidth)
    | Clear (Index TextHeight) (Index TextWidth)
    deriving (Generic, NFDataX)

screenEditor
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> (Signal dom Bool, Signal dom TextCoord, Signal dom (Maybe (TextCoord, Unsigned 8)))
screenEditor = mealyStateB step (Ready 0 0)
  where
    step chr = do
        (ready, write) <- putChar chr
        cursor <- gets $ \case
            Clear y x -> (y, 0)
            Ready y x -> (y, x)
        return (ready, cursor, write)

    putChar chr = get >>= \case
        Clear y x -> do
            put $ maybe (Ready y 0) (Clear y) $ succIdx x
            return (False, Just ((y, x), 0x20))
        Ready y x -> case chr of
            Nothing -> do
                return (True, Nothing)
            Just 0x0a -> do
                put $ Clear (nextIdx y) 0
                return (False, Nothing)
            Just chr -> do
                ready <- case succIdx x of
                    Just x' -> do
                        put $ Ready y x'
                        return True
                    Nothing -> do
                        put $ Clear (nextIdx y) 0
                        return False
                return (ready, Just ((y, x), chr))

video
    :: (HiddenClockResetEnable Dom25)
    => Signal Dom25 TextCoord
    -> Signal Dom25 (Maybe (TextCoord, Unsigned 8))
    -> ( Signal Dom25 Bool
       , VGAOut Dom25 8 8 8
       )
video (fromSignal -> cursor) (fromSignal -> w) = (frameEnd, delayVGA vgaSync rgb)
  where
    VGADriver{..} = vgaDriver vga640x480at60
    frameEnd = isFalling False (isJust <$> vgaY)

    showCursor = regEn True (isRising False $ cnt .==. 0) $ not <$> showCursor
      where
        cnt = regEn (0 :: Index 30) frameEnd $ nextIdx <$> cnt

    visible = fromSignal $ isJust <$> charX .&&. isJust <$> charY
    newChar = fromSignal $ charX ./=. register Nothing charX

    charAddr = do
        new <- newChar
        yx <- charYX
        pure $ do
            guard new
            yx

    charLoad =
        enable (delayI False newChar) $
        delayedRam (blockRam1 ClearOnReset (SNat @(TextWidth * TextHeight)) 0)
               (maybe (0 :: Unsigned (BitSize TextCoord)) bitCoerce <$> charAddr)
               (fmap (\(a, d) -> (bitCoerce a, d)) <$> w)

    glyphAddr = liftA2 (,) <$> charLoad <*> delayI Nothing (fromSignal glyphY)
    glyphLoad = mux (delayI False $ isNothing <$> glyphAddr) (pure Nothing) $
                Just <$> fontRom (fromMaybe (0,0) <$> glyphAddr)

    frame = pure (0x30, 0x30, 0x30)
    fg = pure maxBound
    bg = pure minBound

    (fg', bg') = (mux isCursor bg fg, mux isCursor fg bg)
      where
        isCursor = delayI False $ fromSignal showCursor .&&. charYX .==. (Just <$> cursor)

    rgb = mux (not <$> delayI False visible) frame $
          mux (bitToBool <$> pixel) fg' bg'

    newPixel = fromSignal $ glyphX ./=. register Nothing glyphX
    row = delayedRegister 0x00 $ \row ->
      mux (isJust <$> glyphLoad) (fromJust <$> glyphLoad) $
      mux (delayI False newPixel) ((`shiftR` 1) <$> row) $
      row

    pixel :: DSignal Dom25 3 Bit
    pixel = lsb <$> row

    (charX, glyphX) = scale @TextWidth (SNat @FontWidth) . center $ vgaX
    (charY, glyphY) = scale @TextHeight (SNat @FontHeight) . center $ vgaY
    charYX = fromSignal $ liftA2 (,) <$> charY <*> charX

fontRom :: (HiddenClockResetEnable dom) => DSignal dom n (Unsigned 8, Index 8) -> DSignal dom (n + 1) (Unsigned 8)
fontRom = delayedRom $ fmap unpack . romFilePow2 "font.bin" . fmap toAddr
  where
    toAddr :: (Unsigned 8, Index 8) -> Unsigned (8 + 3)
    toAddr = bitCoerce
