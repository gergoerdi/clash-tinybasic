{-# LANGUAGE LambdaCase #-}
module Hardware.TinyBASIC.Sim where

import Clash.Prelude hiding (lift)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Char (ord, chr, isPrint)
import System.Terminal

sampleEvent
    :: (MonadInput m)
    => m (Maybe (Either Interrupt Event))
sampleEvent = awaitWith $ \int ev -> msum
    [ Just . Left <$> int
    , Just . Right <$> ev
    , return Nothing
    ]

sampleKey
    :: (MonadInput (TerminalT t m), MonadPlus m)
    => TerminalT t m (Maybe (Unsigned 8))
sampleKey = sampleEvent >>= \case
    Just (Left int) -> return $ Just 0x03
    Just (Right (KeyEvent key mods))
        | CharKey c <- key, mods == mempty -> return $ Just . fromIntegral . ord $ c
        | CharKey 'D' <- key, mods .&. ctrlKey /= mempty -> lift mzero
        | EnterKey <- key -> return $ Just . fromIntegral . ord $ '\r'
    _ -> return Nothing

printByte :: (MonadPrinter m) => Unsigned 8 -> m ()
printByte val = case chr . fromIntegral $ val of
    '\r' -> putStringLn ""
    c | isPrint c -> putChar c >> flush
    _  -> return ()
