import Prelude ((^))
import Clash.Prelude hiding ((^), lift)

import Hardware.Intel8080
import Hardware.Intel8080.Model

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Array.IO
import qualified Data.List as L
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)

import Text.Printf
import Data.Char (chr, ord, isPrint)
import System.Terminal
import Control.Concurrent.STM
import System.Exit

import Paths_tinybasic

sampleKey :: (MonadInput m, MonadPrinter m) => MaybeT m (Maybe Value)
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

main :: IO ()
main = do
    romFile <- getDataFileName "image/intel8080/alpha-basic1000.a80.com"
    bs <- BS.unpack <$> BS.readFile romFile
    let memL = L.take (2 ^ 16) $ bs <> L.repeat 0x00
    (arr :: IOArray Addr Value) <- newListArray (minBound, maxBound) (fromIntegral <$> memL)

    let verbose = False

    let getKey = lift $ maybe (liftIO exitSuccess) return =<< runMaybeT sampleKey
        checkInput = do
            queued <- get
            case queued of
                Just prev -> return ()
                Nothing -> put =<< getKey
            gets isJust
        getInput = get <* put Nothing

    let getStatus = do
            inputReady <- checkInput
            let val | inputReady = 0x03
                    | otherwise = 0x02
            when verbose $ liftIO $ printf "<- status 0x%02x\n" val
            return val
        putStatus val = do
            when verbose $ liftIO $ printf "-> status 0x%02x\n" val
            return 0x00

        getData = do
            when verbose $ lift $ putString "<- data "
            -- val <- fromMaybe (error "exit") <$> runMaybeT waitKey
            val <- fromMaybe 0x00 <$> getInput
            when verbose $ liftIO $ printf "0x%02x\n" val
            return val

        putData val = do
            when verbose $ liftIO $ printf "-> data 0x%02x\t" val
            let c = chr . fromIntegral $ val
            lift $ case val of
                0x0d -> putStringLn ""
                _ | isPrint c -> do
                    putChar c >> flush
                    when verbose $ putStringLn ""
                _ -> return ()
            return 0x00

    let inPort port
          | port == statusPort = getStatus
          | port == dataPort = getData
          | otherwise = return 0x00

        outPort port
          | port == statusPort = putStatus
          | port == dataPort = putData
          | otherwise = \_ -> return 0x00

    withTerminal $ runTerminalT $ do
        let w = World
                { readMem = liftIO . readArray arr
                , writeMem = \addr x -> liftIO $ writeArray arr addr x
                , inPort = inPort
                , outPort = outPort
                }
            s = mkState 0x0100
        execStateT (runSoftCPU w s) Nothing

    return ()
  where
    statusPort = 0xde
    dataPort = 0xdf
