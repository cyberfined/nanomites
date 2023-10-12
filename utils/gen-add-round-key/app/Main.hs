module Main (main) where

import           Control.Monad   (forM, forM_)
import           Data.Text       (Text)
import           System.Exit     (exitFailure)
import           System.IO       (stdin, stderr)
import           System.Random   (mkStdGen, newStdGen)

import           Generator
import           OptionsParser
import           Printer

import qualified Data.ByteString as BS
import qualified Data.Text.IO    as TIO

import Numeric (showHex)

main :: IO ()
main = do
    Options{..} <- parseOptions
    funcs <- readKeysAndIvs optFunctions
    gen <- maybe newStdGen (pure . mkStdGen) optSeed
    case generateGetIv gen funcs of
        Left err               -> die (interpretErrorToText err)
        Right (ivCode, newGen) -> case generateAddRoundKey newGen funcs of
            Left err                   -> die (interpretErrorToText err)
            Right (addRoundKeyCode, _) -> TIO.putStr $ printCode addRoundKeyCode ivCode

readKeysAndIvs :: [FunctionOption] -> IO [FuncInfo]
readKeysAndIvs funcs = forM funcs $ \FunctionOption{..} -> do
{-
    iv <- toIv <$> BS.hGet stdin 16 >>= \case
        Nothing       -> die "Failed to read iv"
        Just parsedIv -> pure parsedIv
    key <- toKey <$> BS.hGet stdin 240 >>= \case
        Nothing        -> die "Failed to read key"
        Just parsedKey -> pure parsedKey
-}

    bsIv <- BS.hGet stdin 16
    bsKey <- BS.hGet stdin 240
    Just iv <- pure (toIv bsIv)
    Just key <- pure (toKey bsKey)

    let Address addr = optAddress
    BS.writeFile ("dumps/" ++ showHex addr "" ++ "_iv") bsIv
    BS.writeFile ("dumps/" ++ showHex addr "" ++ "_key") bsKey
    pure $ FuncInfo { funcAddress = optAddress
                    , funcSize    = optSize
                    , funcKey     = key
                    , funcIv      = iv
                    }

die :: Text -> IO a
die err = TIO.hPutStrLn stderr err >> exitFailure
