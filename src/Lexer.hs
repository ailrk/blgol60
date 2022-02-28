{-# LANGUAGE OverloadedStrings #-}
module Lexer where

import           Control.Applicative    (Alternative ((<|>)))
import           Data.Functor.Identity  (Identity)
import qualified Data.Text              as T
import           Position
import qualified Text.Parsec.Char       as P
import           Text.Parsec.Combinator as P
import qualified Text.Parsec.Language   hiding (LanguageDef, emptyDef)
import qualified Text.Parsec.Language   as P
import qualified Text.Parsec.Prim       as P
import qualified Text.Parsec.Text       as P
import           Text.Parsec.Token      as P

-- Algol60 BFN
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm
type LanguageDef st = GenLanguageDef T.Text st Identity

dogeAlgol60Def = LanguageDef
  { commentStart   = "comment"
  , commentEnd     = ""
  , commentLine    = ""
  , nestedComments = True
  , identStart     = P.letter
  , identLetter    = P.alphaNum <|> P.oneOf "_"
  , opStart        = opLetter dogeAlgol60Def
  , opLetter       = P.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames  = ["+", "-", "*", "/", "<", "=<", ">", ">=", "/=", "~", "="]
  , reservedNames= ["begin", "end", "own", "real", "integer", "bool"
                   , "array", "switch", "procedure", "value", "string"
                   , "label", "goto", "if", "then", "else", "do", "for"
                   , "or", "and", "not", "iff" , "step", "while", "comment"
                   , "true", "false"]
  , caseSensitive  = True
  }

-- -- | token type
-- data Token = Token Position (Maybe T.Text) deriving (Eq, Show)

lexer :: GenTokenParser T.Text u IO
lexer = P.makeTokenParser dogeAlgol60Def

identifier = P.identifier lexer

-- keywords
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer

-- numbers
unsignedInteger = P.natural lexer
integer = P.integer lexer
float = P.float lexer

-- string literals
stringLiteral = P.stringLiteral lexer
charLiteral = P.charLiteral lexer

-- separators
whileSpace = P.whiteSpace lexer
semiSep = P.semiSep lexer
semiSep1 = P.semiSep1 lexer
commaSep = P.commaSep lexer
commaSep1 = P.commaSep1 lexer
symbol = P.symbol lexer
