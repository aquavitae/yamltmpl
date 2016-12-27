module Main where

import qualified Optparse
import qualified Data.ByteString.Char8 as BS
import qualified Data.Hashable
import qualified Data.HashMap.Strict   as HashMap
import           Data.Semigroup        ((<>))
import qualified Data.Text             as T
import qualified Data.Vector
import           Data.Yaml


readYamlFile :: FromJSON a => FilePath -> IO a
readYamlFile p = do
    d  <- Data.Yaml.decode <$> BS.readFile p
    case d of
        Nothing -> error $ "Failed to read file " <> p
        Just d' -> return d'


parseValue :: T.Text -> T.Text
parseValue value = T.pack "** THIS WILL BE PARSED **"


change :: Value -> Value
change (Array x)  = Array . fmap change $ x
change (Object x) = Object . fmap change $ x
change (String x) = String . parseValue $ x
change x          = x

yamlTemplate :: Optparse.Arguments -> IO ()
yamlTemplate (Optparse.Arguments file t) = do
  print file
  yamlData <- readYamlFile file :: IO Value
  -- context <- readYamlFile t :: IO Value
  print $ change yamlData


main :: IO ()
main = Optparse.main yamlTemplate
