module Main (main) where

import           Control.Monad   (forM)
import           Data.Text       (Text)
import           System.Exit     (exitFailure)
import           System.IO       (stdin, stderr)
import           System.Random   (mkStdGen, newStdGen)

import           Generator
import           OptionsParser
import           Printer

import qualified Data.ByteString as BS
import qualified Data.Text.IO    as TIO

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
    iv <- toIv <$> BS.hGet stdin 16 >>= \case
        Nothing       -> die "Failed to read iv"
        Just parsedIv -> pure parsedIv
    key <- toKey <$> BS.hGet stdin 240 >>= \case
        Nothing        -> die "Failed to read key"
        Just parsedKey -> pure parsedKey

    pure $ FuncInfo { funcAddress = optAddress
                    , funcSize    = optSize
                    , funcKey     = key
                    , funcIv      = iv
                    }

die :: Text -> IO a
die err = TIO.hPutStrLn stderr err >> exitFailure
