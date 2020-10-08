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

intel8080 :: Rules ()
intel8080 = do
    let binFile = outDir </> "intel8080/image.bin"
    binFile %> \out -> do
        let imageFile = "image/intel8080/alpha-basic1000.a80.com"
        binImage (Just $ 0x8000) imageFile out

    forM_ targets $ \(name, synth, clock) -> do
        kit@ClashKit{..} <- clashRules (outDir </> "intel8080") (name </> "clash") Verilog
            [ "src" ]
            "src/intel8080/board.hs"
            [ "-Wno-partial-type-signatures"
            , "-fclash-inline-limit=600"
            , "-D__NATIVE_CLOCK__=" <> show clock
            ] $
            need [binFile]
        SynthKit{..} <- synth kit (outDir </> "intel8080" </> name </> "synth") ("target" </> name) "TinyBASIC"

        mapM_ (uncurry $ nestedPhony ("intel8080" </> name)) $
          ("clashi", clash ["--interactive", unBuildDir "src/intel8080/board.hs"]) :
          ("bitfile", need [bitfile]):
          phonies

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    intel8080
