import System.Environment (getArgs)
import Text.Megaparsec (runParser, errorBundlePretty)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as L8
import Jass.Parser (programm)

main = do
  j : _ <- getArgs
  x <- runParser programm j <$> readFile j
  case x of
    Left err -> error $ errorBundlePretty err
    Right ast ->
      L8.putStrLn $ encode ast
