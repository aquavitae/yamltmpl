module Main where

import qualified Options.Applicative as O
import Data.Semigroup ((<>))
import Data.Yaml
import qualified Data.Vector
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.ByteString.Char8 as BS

data OutputFormat = Json | Yaml deriving (Enum)

data Arguments = Arguments
  { file  :: String
  , outputFormat  :: String }


options :: O.Parser Arguments
options = Arguments
     <$> O.strOption
        ( O.long "file"
        <> O.short 'f'
        <> O.metavar "FILE"
        <> O.help "Input template file" )
     <*> O.strOption
        ( O.long "outputFormat"
        <> O.short 't'
        <> O.metavar "yaml|json"
        <> O.help "Output format (default: yaml)" )


readYamlFile :: FromJSON a => FilePath -> IO a
readYamlFile p = do
    d  <- Data.Yaml.decode <$> BS.readFile p
    case d of
        Nothing -> error $ "Failed to read file " <> p
        Just d' -> return d'


parseValue :: Value -> Value
parseValue value = 2


applyContext :: FromJSON a => a -> a
applyContext x =
  case x of
    Array _ -> map applyContext x
    Object _ -> HashMap.map applyContext x
    String _ -> parseValue x
    _ -> x


buildTemplate :: Arguments -> IO ()
buildTemplate (Arguments file t) = do
  yamlData <- readYamlFile file :: IO Object
  context <- readYamlFile t :: IO Object
  map applyContext yamlData


main :: IO ()
main = execParser opts >>= buildTemplate
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
