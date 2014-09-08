-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- This module exposes a few parsing combinators that plugin on "parsec"’s
-- ones.
----------------------------------------------------------------------------

module Photon.Core.Parsing (
    -- * Common parsers
    eol
  , line
  , blank
  , blanks
  , blanks1
  , pair
  , sectionIdentifier
  , identifierName
  , comment
  , integralParser
  , unsignedParser
  , floatingParser
    -- * Re-exported
  , module X
  , CharParser
  ) where

import Control.Monad ( void )
import Control.Applicative as X hiding ( optional, many )
import Numeric ( readDec, readFloat, readSigned )
import Text.Parsec as X hiding ( Line, (<|>) )
import Text.ParserCombinators.Parsec.Char ( CharParser )

-- |Parse an end-of-line.
eol :: (Stream s m Char) => ParsecT s u m ()
eol =
        try (void $ string "\n\r")
    <|> void (string "\n")
    <?> "end of line"

-- |Parse a line.
line :: (Stream s m Char) => ParsecT s u m String
line = many (noneOf "\n\r") <* eol

-- |Parse a blank.
blank :: (Stream s m Char) => ParsecT s u m ()
blank = void (oneOf " \t") <?> "blank"

-- |Parse 0 or more blanks.
blanks :: (Stream s m Char) => ParsecT s u m ()
blanks = void (many blank)

-- |Parse 1 or more blanks.
blanks1 :: (Stream s m Char) => ParsecT s u m ()
blanks1 = void (many1 blank)

-- |Parse a pair using two parsers and a separator.
pair :: (Stream s m Char)
     => ParsecT s u m a
     -> String 
     -> ParsecT s u m b
     -> ParsecT s u m (a,b)
pair lp sep rp = do
    a <- lp
    _ <- between blanks blanks (string sep)
    b <- rp
    return (a,b)

-- |Parse a section identifier, e.g. "@sectionIdentifier", and get its name.
sectionIdentifier :: (Stream s m Char) => ParsecT s u m String
sectionIdentifier = do
    _ <- char '@'
    s <- identifierName <?> "section identifier"
    _ <- eol
    return s 

-- |Parse an identifier, removing both leading and trailing spaces..
identifierName :: (Stream s m Char) => ParsecT s u m String
identifierName =  many1 (noneOf "\n\r :;,@[]{}()<>\"") <?> "identifier"

-- |Parse a comment and discard it, because we really don’t give a damned fuck.
comment :: (Stream s m Char) => ParsecT s u m ()
comment = string "--" *> many (noneOf "\n\r") *> eol

-- |Parse an integral value.
integralParser :: (Read a,Integral a,Monad m)
               => ParsecT String u m a
integralParser = numericParser reads <?> "integral number"

-- |Parse an unsigned value.
unsignedParser :: (Read a,Integral a,Monad m)
               => ParsecT String u m a
unsignedParser = numericParser readDec <?> "natural number"

-- |Parse a floating number.
floatingParser :: (Read a,RealFrac a,Monad m)
               => ParsecT String u m a
floatingParser = numericParser (readSigned readFloat) <?> "floating number"

-- Generic parser for parsing `Num`.
numericParser :: (Read a,Num a,Monad m)
              => ReadS a
              -> ParsecT String u m a
numericParser r = do
    i <- getInput
    case r i of
      [(n,s)] -> setInput s *> return n
      _       -> parserZero
