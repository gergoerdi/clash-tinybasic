{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
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
    , clashDir = "clash-syn"
    }

main :: IO ()
main = clashShake clashProject $ do
    ClashProject{..} <- ask
    let synDir = buildDir </> clashDir

    kit@ClashKit{..} <- clashRules Verilog "src" $ do
        need [buildDir </> "image.bin"]

    -- xilinxISE kit papilioPro "target/papilio-pro" "papilio-pro"
    -- xilinxISE kit papilioOne "target/papilio-one" "papilio-one"
    xilinxVivado kit nexysA750T "target/nexys-a7-50t" "nexys-a7-50t"

    lift $ do
      buildDir </> "image.bin" %> \out -> do
          let imageFile = "image/intel8080/alpha-basic1000.a80.com"
          binImage (Just $ 0x1000 - 0x0200) imageFile out
