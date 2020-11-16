import Prelude ((^))
import Clash.Prelude hiding ((^), lift)

import Hardware.Intel8080
import Hardware.Intel8080.Model
import Hardware.TinyBASIC.Sim

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Array.IO
import qualified Data.List as L
import qualified Data.ByteString as BS

import Text.Printf
import Data.Char (chr, ord, isPrint)
import System.Terminal

import Paths_tinybasic

main :: IO ()
main = do
    (arr :: IOArray Addr Value) <- newArray (minBound, maxBound) 0x00

    romFile <- getDataFileName "image/intel8080/tinybasic-2.0.bin"
    bs <- fmap fromIntegral . BS.unpack <$> BS.readFile romFile
    zipWithM_ (writeArray arr) [0x0000..] bs

    let verbose = False

    let checkInput = do
            queued <- get
            case queued of
                Just prev -> return ()
                Nothing -> put =<< lift sampleKey
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

    runMaybeT $ withTerminal $ runTerminalT $ do
        let w = World
                { readMem = liftIO . readArray arr
                , writeMem = \addr x -> liftIO $ writeArray arr addr x
                , inPort = inPort
                , outPort = outPort
                }
            s = mkState 0x0000
        execStateT (runSoftCPU w s) Nothing

    return ()
  where
    statusPort = 0x10
    dataPort = 0x11
