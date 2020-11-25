import Clash.Prelude

import RetroClash.Sim.IO
import Hardware.TinyBASIC.Sim
import Hardware.TinyBASIC.Intel8080

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import System.Terminal

main :: IO ()
main = do
    sim <- simulateIO_ @System (uncurry logicBoard . unbundle) (Nothing, True)

    runMaybeT $ withTerminal $ runTerminalT $ forever $ sim $ \outByte -> do
        traverse_ printByte outByte
        inByte <- sampleKey
        return (inByte, True)
    return ()
