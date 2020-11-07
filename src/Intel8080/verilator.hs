{-# LANGUAGE RecordWildCards, OverloadedStrings, NumericUnderscores #-}
module Main where

import Clash.Prelude hiding (lift)

import RetroClash.VGA
import RetroClash.Sim.SDL
import RetroClash.Sim.VGA
import RetroClash.Sim.VGASDL

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.State
import Data.Array.IO
import Control.Lens

import SDL (ticks)
import Data.Int
import Text.Printf
import Control.Monad.Loops
import Data.Text

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    buf <- newBufferArray

    flip evalStateT initSink $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        let input = INPUT
                { iRESET = low
                , iRX = low
                }

        untilM_ (return ()) $ do
            vgaOut <- do
                OUTPUT{..} <- liftIO $ runCycle input
                return (oVGA_HSYNC, oVGA_VSYNC, (oVGA_RED, oVGA_GREEN, oVGA_BLUE))
            vgaSinkBuf vga640x480at60 buf vgaOut

        return $ rasterizeBuffer buf
  where
    videoParams = MkVideoParams
        { windowTitle = "TinyBASIC/i8080"
        , screenScale = 2
        , screenRefreshRate = 60
        , reportFPS = True
        }
