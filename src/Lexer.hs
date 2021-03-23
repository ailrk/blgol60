module Lexer
  ( def
  , lexer
  )
where

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
  , makeTokenParser
  )


-- Algol60 BFN
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm

def :: (Monad m) => P.GenLanguageDef Text u m
def =
  P.LanguageDef
    { commentStart = "comment"
    , commentEnd = ";"
    , commentLine = ""
    , nestedComments = False
    , identStart = P.letter
    , identLetter = P.alphaNum <|> P.oneOf "_"
    , opStart = opLetter def
    , opLetter = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = ["+", "-", "*", "/", "<", "=<", ">", ">=", "~=", "~", "="]
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


lexer :: (Monad m) => P.GenTokenParser Text u m
lexer = P.makeTokenParser def
