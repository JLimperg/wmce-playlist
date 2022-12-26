module WMCE (Album(..), parseCharts) where

import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
    ( parse,
      errorBundlePretty,
      some,
      eof,
      Parsec,
      MonadParsec(takeWhile1P, takeWhileP) )
import Text.Megaparsec.Char ( char, hspace, eol )

type Parser = Parsec Void Text

data Album = Album
  { albumTitle :: Text
  , albumArtists :: [Text]
  } deriving (Read, Show, Eq, Ord)

sepChar :: Char
sepChar = '•'

untilSep :: Parser Text
untilSep = takeWhileP Nothing (/= sepChar) <* char sepChar

skipUntilNewline :: Parser ()
skipUntilNewline =
  takeWhileP Nothing (\c -> not $ c == '\n' || c == '\r') >> void eol

album :: Parser Album
album = do
  _ <- takeWhile1P (Just "digit") isDigit >> hspace
  albumTitle <- Text.strip <$> untilSep
  albumArtists1 <- untilSep
  let albumArtists = map Text.strip $ Text.split (== '&') albumArtists1
  skipUntilNewline
  pure Album { albumTitle, albumArtists }

charts :: Parser [Album]
charts = some album <* eof

parseCharts :: Text -> Either String [Album]
parseCharts = first errorBundlePretty . parse charts ""
