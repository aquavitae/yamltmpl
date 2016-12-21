import Options.Applicative
import Data.Semigroup ((<>))
import Data.Yaml
import Data.Vector
import Data.HashMap.Lazy as HashMap
import qualified Data.ByteString.Char8 as BS

data OutputFormat = Json | Yaml deriving (Enum)

data Arguments = Arguments
  { file  :: String
  , outputFormat  :: String }

options :: Options.Applicative.Parser Arguments
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


readYamlFile :: FromJSON a => FilePath -> IO a
readYamlFile p = do
    d  <- Data.Yaml.decode <$> BS.readFile p
    case d of
        Nothing -> error $ "Failed to read file " <> p
        Just d' -> return d'

applyContext :: a -> a
applyContext x =
  case x of
    Array _ -> Data.Vector.map applyContext x
    Object _ -> HashMap.map applyContext x
    -- Value -> id x
    _ -> x


buildTemplate :: Arguments -> IO ()
buildTemplate (Arguments file t) = do
  yamlData <- readYamlFile file :: IO Object
  context <- readYamlFile t :: IO Object
  Data.Vector.map applyContext yamlData

main :: IO ()
main = execParser opts >>= buildTemplate
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
