{-# LANGUAGE LambdaCase #-}
module Hardware.ACIA where

import Clash.Prelude

import RetroClash.Utils
import RetroClash.Port

import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Data.Traversable (for)
import Control.Arrow (second)

-- Asynchronous Communications Interface Adapter
acia
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom Bool
    -> Signal dom (Maybe (PortCommand (Unsigned 1) (Unsigned 8)))
    -> (Signal dom (Unsigned 8), Signal dom (Maybe (Unsigned 8)))
acia inByte outReady cmd = mealyStateB step Nothing (inByte, outReady, cmd)
  where
    step (inByte, outReady, cmd) = fmap (second getFirst) . runWriterT $ do
        traverse (put . Just) inByte
        fmap fromJustX $ for cmd $ \case
            ReadPort 0x0 -> do
                inReady <- isJust <$> get
                return $ bitCoerce $
                    False :>      -- TODO: IRQ
                    False :>      -- TODO: parity error
                    False :>      -- TODO: receiver overrun
                    False :>      -- TODO: framing error
                    False :>      -- TODO: CTS
                    False :>      -- TODO: DCD
                    outReady :>
                    inReady :>
                    Nil
            WritePort 0x0 x -> do
                return 0x00
            ReadPort 0x1 -> do
                queued <- get <* put Nothing
                return $ fromMaybe 0x00 queued
            WritePort 0x1 x -> do
                tell $ pure x
                return 0x00
