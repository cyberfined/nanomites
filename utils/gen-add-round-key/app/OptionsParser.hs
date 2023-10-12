module OptionsParser
    ( Options(..)
    , FunctionOption(..)
    , parseOptions
    ) where

import           Data.Word           (Word32)
import           Options.Applicative

import           Generator           (Address (..))

data Options = Options
    { optSeed      :: !(Maybe Int)
    , optFunctions :: ![FunctionOption]
    }

data FunctionOption = FunctionOption
    { optAddress :: !Address
    , optSize    :: !Word32
    }

parseOptions :: IO Options
parseOptions = execParser (info optionsParser fullDesc)

optionsParser :: Parser Options
optionsParser = options <**> helper

options :: Parser Options
options =  Options
       <$> (optional . option auto)
           ( long "seed"
          <> short 'S'
          <> help "seed"
           )
       <*> functions

functions :: Parser [FunctionOption]
functions = some $ FunctionOption
         <$> option auto
             ( long "address"
            <> short 'a'
            <> help "function address"
             )
         <*> option auto
             ( long "size"
            <> short 's'
            <> help "function size"
             )
