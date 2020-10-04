{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
import Clash.Shake

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

clashProject = ClashProject
    { projectName = "TinyBASIC"
    , clashModule = "intel8080/board"
    , clashTopName = "topEntity"
    , topName = "Top"
    , clashFlags =
        [ "-i../retroclash-lib/src"
        , "-Wno-partial-type-signatures"
        , "-fclash-inline-limit=600"
        ]
    , buildDir = "_build"
    }

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T, 100_000_000)
    , ("papilio-pro",  xilinxISE papilioPro,     32_000_000)
    , ("papilio-one",  xilinxISE papilioOne,     32_000_000)
    ]

main :: IO ()
main = clashShake clashProject $ do
    ClashProject{..} <- ask

    img <- do
        let binFile = buildDir </> "image-i8080.bin"
        lift $ binFile %> \out -> do
            let imageFile = "image/intel8080/alpha-basic1000.a80.com"
            binImage (Just $ 0x10000) imageFile out
        return $ need [binFile]

    forM_ targets $ \(name, synth, clock) -> do
        let cpp = [ ("__NATIVE_CLOCK__", show clock) ]
        kit@ClashKit{..} <- clashRules Verilog (name </> "clash") "src" cpp img
        synth kit name ("target" </> name)
