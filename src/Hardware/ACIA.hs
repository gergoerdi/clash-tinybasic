module Hardware.ACIA where

import Clash.Prelude

import RetroClash.Utils
import RetroClash.Port

import Data.Maybe
import Control.Monad.State

-- Asynchronous Communications Interface Adapter
acia
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> Signal dom Bool
    -> Signal dom (Maybe (PortCommand (Unsigned 1) (Unsigned 8)))
    -> (Signal dom (Maybe (Unsigned 8)), Signal dom (Maybe (Unsigned 8)))
acia inByte outReady cmd = mealyStateB step Nothing (inByte, outReady, cmd)
  where
    step (inByte, outReady, cmd) = do
        traverse (put . Just) inByte
        case cmd of
            Nothing ->
                return (Nothing, Nothing)

            Just (ReadPort 0x0) -> do
                inReady <- isJust <$> get
                let val = (if inReady then 0x01 else 0x00) .|.
                          (if outReady then 0x02 else 0x00)
                return (Just val, Nothing)
            Just (WritePort 0x0 x) -> do
                return (Just 0x00, Nothing)

            Just (ReadPort 0x1) -> do
                queued <- get <* put Nothing
                return (Just $ fromMaybe 0x00 queued, Nothing)
            Just (WritePort 0x1 x) -> do
                return (Just 0x00, Just x)
