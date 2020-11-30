import Clash.Prelude hiding (lift)

import Hardware.Intel8080
import Hardware.Intel8080.Model
import Hardware.TinyBASIC.Sim

import System.Terminal
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Array.IO
import qualified Data.ByteString as BS

import Paths_tinybasic

main :: IO ()
main = do
    (arr :: IOArray Addr Value) <- newArray (minBound, maxBound) 0x00

    romFile <- getDataFileName "image/intel8080/tinybasic-2.0.bin"
    bs <- fmap fromIntegral . BS.unpack <$> BS.readFile romFile
    zipWithM_ (writeArray arr) [0x0000..] bs

    let checkInput = do
            queued <- get
            when (isNothing queued) $ lift sampleKey >>= put
            gets isJust
        getInput = get <* put Nothing

    let getStatus = do
            inputReady <- checkInput
            let val | inputReady = 0x03
                    | otherwise = 0x02
            return val
        putStatus val = return ()

        getData = fromMaybe 0x00 <$> getInput
        putData val = lift $ printByte val

    let inPort port
          | port == statusPort = getStatus
          | port == dataPort = getData
          | otherwise = return 0x00

        outPort port x
          | port == statusPort = putStatus x >> return 0x00
          | port == dataPort = putData x >> return 0x00
          | otherwise = return 0x00

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
