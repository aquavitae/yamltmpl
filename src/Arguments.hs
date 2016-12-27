module Arguments (
  main
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Text
import qualified Data.Vector
import           Options.Applicative

data OutputFormat = Json | Yaml deriving (Enum)

data Arguments = Arguments
  { file         :: String
  , outputFormat :: String }


options :: Parser Arguments
options = Arguments
     <$> strOption
        ( long "file"
        <> short 'f'
        <> metavar "FILE"
        <> help "Input template file" )
     <*> strOption
        ( long "outputFormat"
        <> short 't'
        <> metavar "yaml|json"
        <> help "Output format (default: yaml)" )


main :: (Arguments -> IO ()) -> IO ()
main processor = execParser opts >>= processor
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
