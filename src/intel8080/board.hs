{-# LANGUAGE RecordWildCards, ApplicativeDo, NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}

import Clash.Prelude hiding (ram, rom)
import Clash.Annotations.TH

import Hardware.Intel8080
import Hardware.Intel8080.CPU
import Hardware.ACIA

import RetroClash.Utils
import RetroClash.CPU
import RetroClash.Clock
import RetroClash.Port
-- import RetroClash.Memory

import Data.Maybe
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Writer

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

        interruptRequest = pure False

        (dataIn, (tx, ())) = memoryMap (Just <$> _addrOut) _dataOut $ ports <||> mem
          where
            ports = do
                tx <- mask 0xde $ port $ acia (SNat @9600) rx
                return tx

            mem = do
                mask @15 0x0000 $ rom $ fmap unpack . romFilePow2 "image.bin"
                mask @15 0x8000 $ ram $ blockRamU ClearOnReset (SNat @0x8000) (const 0)

newtype CollectedSignal dom a = CollectedSignal{ getCollectedSignal :: Signal dom (Maybe a) }

instance Semigroup (CollectedSignal dom a) where
    x <> y = CollectedSignal $ mplus <$> getCollectedSignal x <*> getCollectedSignal y

instance Monoid (CollectedSignal dom a) where
    mempty = CollectedSignal $ pure Nothing

type Addressing dom addr dat res =
    ReaderT
      (Signal dom (Maybe addr), Signal dom (Maybe dat))
      (Writer (CollectedSignal dom res))

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> Addressing dom addr dat res out
    -> (Signal dom (Maybe res), out)
memoryMap addr write spec = (result, output)
  where
    (output, getCollectedSignal -> result) = runWriter $ runReaderT spec (addr, write)

mapAddr
    :: (HiddenClockResetEnable dom)
    => (addr -> Maybe addr') -> Addressing dom addr' dat out r -> Addressing dom addr dat out r
mapAddr f body = withReaderT (first $ fmap (f =<<)) $ do
    (addr, _) <- ask
    censor (CollectedSignal . fmap join . enable (delay False $ isJust <$> addr) . getCollectedSignal) body

infix 1 <||>
(<||>)
    :: (HiddenClockResetEnable dom)
    => Addressing dom addr1 dat out r1
    -> Addressing dom addr2 dat out r2
    -> Addressing dom (Either addr1 addr2) dat out (r1, r2)
body1 <||> body2 = do
    x <- mapAddr (either Just (const Nothing)) body1
    y <- mapAddr (either (const Nothing) Just) body2
    return (x, y)

mask
    :: (KnownNat k, KnownNat n)
    => (HiddenClockResetEnable dom)
    => Unsigned (n + k)
    -> Addressing dom (Unsigned k)       dat out r
    -> Addressing dom (Unsigned (n + k)) dat out r
mask base = mapAddr (maskAddr base)

type RAM dom a d = Signal dom a -> Signal dom (Maybe (a, d)) -> Signal dom d
type ROM dom a d = Signal dom a ->                              Signal dom d

ram :: RAM dom addr dat -> Addressing dom addr dat dat ()
ram mkRam = do
    (addr, w) <- ask
    let output = mkRam (fromJust <$> addr) (liftA2 (,) <$> addr <*> w)
    tell $ CollectedSignal $ Just <$> output

rom :: ROM dom addr dat -> Addressing dom addr w dat ()
rom mkRom = do
    (addr, w) <- ask
    let output = mkRom (fromJust <$> addr)
    tell $ CollectedSignal $ Just <$> output

port
    :: (HiddenClockResetEnable dom, NFDataX out)
    => (Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe out), r))
    -> Addressing dom addr dat out r
port mkPort = do
    (addr, w) <- ask
    let cmd = do
            addr <- addr
            w <- w
            pure $ case (addr, w) of
                (Just addr, Nothing) -> Just $ ReadPort addr
                (Just addr, Just w) -> Just $ WritePort addr w
                _ -> Nothing
    let (output, result) = mkPort cmd
    tell $ CollectedSignal $ delay Nothing output
    return result

makeTopEntity 'topEntity
