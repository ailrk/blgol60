module Lexer where

import Control.Applicative (Alternative ((<|>)))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec (ParsecT)
import Text.Parsec.Char qualified as P
import Text.Parsec.Token as P
  ( GenLanguageDef
      ( LanguageDef
      , caseSensitive
      , commentEnd
      , commentLine
      , commentStart
      , identLetter
      , identStart
      , nestedComments
      , opLetter
      , opStart
      , reservedNames
      , reservedOpNames
      )
  , GenTokenParser
    ( charLiteral
    , commaSep
    , commaSep1
    , float
    , identifier
    , integer
    , natural
    , reserved
    , reservedOp
    , semiSep
    , semiSep1
    , stringLiteral
    , symbol
    , whiteSpace
    )
  , makeTokenParser
  )


-- Algol60 BFN
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm

dogeAlgol60Def :: Monad m => GenLanguageDef Text u m
dogeAlgol60Def =
  LanguageDef
    { commentStart = "comment"
    , commentEnd = ""
    , commentLine = ""
    , nestedComments = True
    , identStart = P.letter
    , identLetter = P.alphaNum <|> P.oneOf "_"
    , opStart = opLetter dogeAlgol60Def
    , opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = ["+", "-", "*", "/", "<", "=<", ">", ">=", "/=", "~", "="]
    , reservedNames =
        [ "begin"
        , "end"
        , "own"
        , "real"
        , "integer"
        , "bool"
        , "array"
        , "switch"
        , "procedure"
        , "value"
        , "string"
        , "label"
        , "goto"
        , "if"
        , "then"
        , "else"
        , "do"
        , "for"
        , "or"
        , "and"
        , "not"
        , "iff"
        , "step"
        , "while"
        , "comment"
        , "true"
        , "false"
        ]
    , caseSensitive = True
    }


-- -- | token type
-- data Token = Token Position (Maybe T.Text) deriving (Eq, Show)

lexer :: Monad m => GenTokenParser Text u m
lexer = P.makeTokenParser dogeAlgol60Def


identifier :: Monad m => ParsecT Text u m Text
identifier = Text.pack <$> P.identifier lexer


-- keywords
reserved :: Monad m => Text -> ParsecT Text u m ()
reserved = P.reserved lexer . Text.unpack


reservedOp :: Monad m => Text -> ParsecT Text u m ()
reservedOp = P.reservedOp lexer . Text.unpack


-- numbers
unsignedInteger :: Monad m => ParsecT Text u m Integer
unsignedInteger = P.natural lexer


integer :: Monad m => ParsecT Text u m Integer
integer = P.integer lexer


float :: Monad m => ParsecT Text u m Double
float = P.float lexer


-- string literals
stringLiteral :: Monad m => ParsecT Text u m Text
stringLiteral = Text.pack <$> P.stringLiteral lexer


charLiteral :: Monad m => ParsecT Text u m Char
charLiteral = P.charLiteral lexer


-- separators
whileSpace :: Monad m => ParsecT Text u m ()
whileSpace = P.whiteSpace lexer


semiSep :: Monad m => ParsecT Text u m a -> ParsecT Text u m [a]
semiSep = P.semiSep lexer


semiSep1 :: Monad m => ParsecT Text u m a -> ParsecT Text u m [a]
semiSep1 = P.semiSep1 lexer


commaSep :: Monad m => ParsecT Text u m a -> ParsecT Text u m [a]
commaSep = P.commaSep lexer


commaSep1 :: Monad m => ParsecT Text u m a -> ParsecT Text u m [a]
commaSep1 = P.commaSep1 lexer


symbol :: Monad m => Text -> ParsecT Text u m String
symbol = P.symbol lexer . Text.unpack
