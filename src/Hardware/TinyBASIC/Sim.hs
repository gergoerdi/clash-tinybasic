module Hardware.TinyBASIC.Sim where

import Clash.Prelude hiding (lift)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Text.Printf
import Data.Char (ord, chr, isPrint)
import System.Terminal
import Control.Concurrent.STM

sampleKey :: (MonadInput m, MonadPrinter m) => MaybeT m (Maybe (Unsigned 8))
sampleKey = do
    lift flush
    ev <- MaybeT $ awaitWith $ \int ev -> msum
        [ Nothing <$ int
        , Just . Just <$> ev
        , Just Nothing <$ return ()
        ]
    return $ case ev of
        Just (KeyEvent (CharKey c) mods) | mods == mempty -> Just $ fromIntegral . ord $ c
        Just (KeyEvent EnterKey _) -> Just 0x0d
        _ -> Nothing

printByte :: (MonadPrinter m) => Unsigned 8 -> m ()
printByte val = case val of
    0x0d -> putStringLn ""
    _ | isPrint c -> putChar c >> flush
      | otherwise -> return ()
  where
    c = chr . fromIntegral $ val
