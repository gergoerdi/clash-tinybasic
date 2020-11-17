{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Config
import Development.Shake.Util

import Clash.Prelude hiding (lift)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Word
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Trans.Class

-- import Paths_tinybasic -- XXX

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T, 100_000_000)
    , ("papilio-pro",  xilinxISE papilioPro,     32_000_000)
    , ("papilio-one",  xilinxISE papilioOne,     32_000_000)
    ]

outDir = "_build"

fontFile = outDir </> "font.bin"

intel8080 :: Rules ()
intel8080 = do
    let binFile = outDir </> "intel8080/image.bin"
    binFile %> \out -> do
        let imageFile = "image/intel8080/tinybasic-2.0.bin"
        binImage (Just $ 0x0800) imageFile out

    do
        let targetDir = outDir </> "intel8080/video"

        kit@ClashKit{..} <- clashRules (targetDir </> "clash") Verilog
            [ "src" ]
            "src/intel8080/video-board.hs"
            [ "-Wno-partial-type-signatures"
            , "-fclash-inline-limit=600"
            ] $
            need [binFile, fontFile]

        forM_ targets $ \(name, synth, _) -> do
            SynthKit{..} <- synth kit (targetDir </> name </> "synth") ("target" </> name </> "video") "TinyBASICVideo"

            mapM_ (uncurry $ nestedPhony ("intel8080/video" </> name)) $
              ("bitfile", need [bitfile]):
              phonies

        nestedPhony ("intel8080/video") "clashi" $
          clash ["--interactive", "src/intel8080/video-board.hs"]

    forM_ targets $ \(name, synth, clock) -> do
        let targetDir = outDir </> "intel8080/serial" </> name

        kit@ClashKit{..} <- clashRules (targetDir </> "clash") Verilog
            [ "src" ]
            "src/intel8080/serial-board.hs"
            [ "-Wno-partial-type-signatures"
            , "-fclash-inline-limit=600"
            , "-D__NATIVE_CLOCK__=" <> show clock
            ] $
            need [binFile]
        SynthKit{..} <- synth kit (targetDir </> "synth") ("target" </> name </> "serial") "TinyBASICSerial"

        mapM_ (uncurry $ nestedPhony ("intel8080/serial" </> name)) $
          ("clashi", clash ["--interactive", "src/intel8080/serial-board.hs"]) :
          ("bitfile", need [bitfile]):
          phonies

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    fontFile %> \out -> do
        let imageFile = "image/font.dat"
        binImage (Just $ 8 * 256) imageFile out

    intel8080
