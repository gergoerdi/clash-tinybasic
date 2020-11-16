module Hardware.TinyBASIC.Sim where

import Clash.Prelude hiding (lift)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Char (ord, chr, isPrint)
import System.Terminal

sampleKey :: (MonadInput m, MonadPrinter m) => MaybeT m (Maybe (Unsigned 8))
sampleKey = do
    lift flush
    ev <- lift $ awaitWith $ \int ev -> msum
        [ Just (KeyEvent (CharKey 'C') ctrlKey) <$ int
        , Just <$> ev
        , Nothing <$ return ()
        ]
    case ev of
        Just (KeyEvent key mods)
          | CharKey c <- key, mods == mempty -> return $ Just $ fromIntegral . ord $ c
          | CharKey 'C' <- key, mods .&. ctrlKey /= mempty -> return $ Just 0x03
          | CharKey 'D' <- key, mods .&. ctrlKey /= mempty -> mzero
          | EnterKey <- key -> return $ Just 0x0d
        _ -> return Nothing

printByte :: (MonadPrinter m) => Unsigned 8 -> m ()
printByte val = case val of
    0x0d -> putStringLn ""
    _ | isPrint c -> putChar c >> flush
      | otherwise -> return ()
  where
    c = chr . fromIntegral $ val
