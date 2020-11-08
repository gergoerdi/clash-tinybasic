{-# LANGUAGE RecordWildCards, NumericUnderscores, LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
import Clash.Prelude hiding (rom)
import Clash.Annotations.TH

import Hardware.Intel8080 hiding (ShiftRotate(..))
import Hardware.Intel8080.CPU
import Hardware.ACIA
import Hardware.Video

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Clock
import RetroClash.Port
import RetroClash.Memory
import RetroClash.SerialRx
import RetroClash.VGA
import RetroClash.PS2
import RetroClash.PS2.ASCII

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Lens
import Data.Traversable

topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "RX"        ::: Signal Dom25 Bit
    -> "PS2"       ::: PS2 Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board rx ps2 = vga
      where
        (frameEnd, vga) = video cursor vidWrite
        (vidReady, cursor, vidWrite) = screenEditor outByte

        cpuOut@CPUOut{..} = mealyCPU (initState 0x0000) defaultOut cpu CPUIn{..}

        interruptRequest = pure False

        -- inByte = fmap unpack <$> serialRx @8 (SNat @9600) rx
        inByte = keyboard ps2

        (dataIn, (outByte, ())) = memoryMap _addrOut _dataOut $ ports <||> mem
          where
            ports = do
                tx <- mask 0xde $ port $ acia inByte vidReady
                return tx

            mem = do
                mask @15 0x0000 $ readOnly $ fmap unpack . romFilePow2 "_build/intel8080/image.bin"
                mask @15 0x8000 $ readWrite $ blockRamU ClearOnReset (SNat @0x8000) (const 0)

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

makeTopEntity 'topEntity
