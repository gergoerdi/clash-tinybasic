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

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T, 100_000_000)
    , ("papilio-pro",  xilinxISE papilioPro,     32_000_000)
    , ("papilio-one",  xilinxISE papilioOne,     32_000_000)
    ]

intel8080 :: Rules ()
intel8080 = flip runReaderT "_build/intel8080" $ do
    img <- do
        let binFile = "_build/intel8080/image.bin"
        lift $ binFile %> \out -> do
            let imageFile = "image/intel8080/alpha-basic1000.a80.com"
            binImage (Just $ 0x8000) imageFile out
        return $ need [binFile]

    forM_ targets $ \(name, synth, clock) -> do
        kit@ClashKit{..} <- clashRules Verilog (name </> "clash") "src/intel8080/board.hs"
            [ "src" ]
            [ "-Wno-partial-type-signatures"
            , "-fclash-inline-limit=600"
            , "-D__NATIVE_CLOCK__=" <> show clock
            ]
            img
        SynthKit{..} <- synth kit name ("target" </> name) "TinyBASIC"

        lift $ mapM_ (uncurry $ nestedPhony ("intel8080" </> name)) $
          ("clashi", clash ["--interactive", unBuildDir "src/intel8080/board.hs"]) :
          ("bitfile", need [bitfile]):
          phonies

main :: IO ()
main = clashShakeMain "_build" $ do
    intel8080
