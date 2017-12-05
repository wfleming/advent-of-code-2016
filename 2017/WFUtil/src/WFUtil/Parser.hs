module WFUtil.Parser where

import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L

type ParseResult a = Either (ParseError (Token String) Dec) a

parseStr :: Parser a -> String -> ParseResult a
parseStr p = parse p "NO_INPUT_FILE"

-- using this after `sepBy` hangs. Why?
skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

eolOrEof :: Parser ()
eolOrEof = (eol >> mempty) <|> eof

words :: Parser [String]
words = (many alphaNumChar) `sepBy` skipSpaces
-- words = many (L.lexeme skipSpaces (many alphaNumChar))

lineSeparated :: Parser a -> Parser [a]
lineSeparated p = p `sepBy` eolOrEof
