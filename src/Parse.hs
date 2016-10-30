{-# LANGUAGE OverloadedStrings #-}
module Parse where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Text.Parsec as P

import Types

many1 :: P.Parsec s u Char -> P.Parsec s u T.Text
many1 = fmap T.pack . P.many

parseDecimal :: P.Parsec T.Text () Int
parseDecimal =
  do eitherDec <- TR.decimal <$> many1 P.digit
     case eitherDec of
       Left err -> fail err
       Right (a, "") -> return a
       Right (a, rest) -> fail $
                          "Failed to parse decimal from digits. "
                          <> "Got " <> show a
                          <> "but " <> T.unpack rest <> " remains."

parseTuple :: P.Parsec T.Text () (Int, Int)
parseTuple =
  (,) <$> dim <* sep <*> dim
  where
    dim = parseDecimal
    sep = P.char 'x'

parseTitle :: P.Parsec T.Text () T.Text
parseTitle =
  P.string "output"
  *> (P.char ' ' *> many1 P.alphaNum)
  <* P.newline

parseMode :: P.Parsec T.Text () (Int, Int)
parseMode =
  P.string "mode"
  *> (P.space *> parseTuple)
  <* P.newline

parsePos :: P.Parsec T.Text () (Int, Int)
parsePos =
  P.string "pos"
  *> (P.space *> parseTuple)
  <* P.newline

parseOff :: P.Parsec T.Text () ()
parseOff =
  void $ P.string "off"

parseNewlineOrEof :: P.Parsec T.Text () ()
parseNewlineOrEof =
  void P.newline
  <|> P.eof

skipLine :: P.Parsec T.Text () ()
skipLine =
  void $ many (P.noneOf "\n")

parseScreen :: P.Parsec T.Text () (Maybe Screen)
parseScreen =
  (Just <$> P.try real)
  <|> (off *> pure Nothing)
  where
    real =
      Screen
      <$> parseTitle
      <* (skipLine *> P.newline)
      <*> parseMode
      <*> parsePos
      <* (skipLine *> parseNewlineOrEof)
    off =
      parseTitle
      *> parseOff *> parseNewlineOrEof

parseScreens :: P.Parsec T.Text () [Screen]
parseScreens = catMaybes <$> many parseScreen
