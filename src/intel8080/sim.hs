import Prelude ((^))
import Clash.Prelude hiding ((^), lift)
import RetroClash.Sim.IO
import Hardware.TinyBASIC.Sim

import Hardware.TinyBASIC.Intel8080

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)

import Text.Printf
import Data.Char (chr, isPrint)
import System.Terminal
import System.Exit

main :: IO ()
main = do
    sim <- simulateIO_ @System (uncurry logicBoard . unbundle) (Nothing, True)

    withTerminal $ runTerminalT $ runMaybeT $ forever $ sim $ \outByte -> do
        traverse_ (lift . printByte) outByte
        inByte <- sampleKey
        return (inByte, True)
    return ()
