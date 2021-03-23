{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE MultiWayIf #-}

module Parser where

import AST
  ( ArraySegment (..)
  , BlockStmt (..)
  , CallStmt (..)
  , Dec (..)
  , EvalStrat (..)
  , Expr
    ( BinopExpr
    , CallExpr
    , IfExpr
    , IntExpr
    , LabelExpr
    , RealExpr
    , StringExpr
    , SwitchExpr
    , UnopExpr
    , VarExpr
    )
  , ForListElement (..)
  , ForStmt (..)
  , IfStmt (..)
  , Parameter (..)
  , ProcedureDec (..)
  , Stmt (..)
  , Type (..)
  , Var (..)
  , toBinaryOp
  , toUnaryOp, BinaryOp (..)
  )
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Lexer qualified
import Position (getPosFromParsec)
import Symbol (Symbol)
import Symbol qualified
import Text.Parsec (ParsecT, anyChar, between, choice, many1, chainl1, satisfy, sepBy, sepBy1, sepEndBy, try, parserTrace, lookAhead, getParserState, State (..), sepEndBy1)
import Text.Parsec.Expr
  ( Assoc (AssocLeft)
  , Operator (Infix, Prefix)
  , buildExpressionParser
  )
import Text.Parsec.Token qualified as P
import Text.Parsec.Char (letter)

import Text.Printf

-- Algol60 BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm
-- http://www.bitsavers.org/pdf/sds/9xx/lang/900699C_Algol60_Ref_Nov66.pdf

type MonadParse m = (HasCallStack, Monad m)


----------------------------------------
-- basics
----------------------------------------


identifier :: (Monad m) => ParsecT Text u m Text
identifier = Text.pack <$> P.identifier Lexer.lexer


-- keywords
reserved :: (Monad m) => Text -> ParsecT Text u m ()
reserved = P.reserved Lexer.lexer . Text.unpack


reservedOp :: (Monad m) => Text -> ParsecT Text u m ()
reservedOp = P.reservedOp Lexer.lexer . Text.unpack


-- numbers
unsignedInteger :: (Monad m) => ParsecT Text u m Integer
unsignedInteger = P.natural Lexer.lexer


integer :: (Monad m) => ParsecT Text u m Integer
integer = P.integer Lexer.lexer


float :: (Monad m) => ParsecT Text u m Double
float = P.float Lexer.lexer


-- string literals
stringLiteral :: (Monad m) => ParsecT Text u m Text
stringLiteral = do
  Text.pack <$> P.stringLiteral Lexer.lexer


charLiteral :: (Monad m) => ParsecT Text u m Char
charLiteral = P.charLiteral Lexer.lexer


-- separators
whileSpace :: (Monad m) => ParsecT Text u m ()
whileSpace = P.whiteSpace Lexer.lexer


semiSep :: (Monad m) => ParsecT Text u m a -> ParsecT Text u m [a]
semiSep = P.semiSep Lexer.lexer


semiSep1 :: (Monad m) => ParsecT Text u m a -> ParsecT Text u m [a]
semiSep1 = P.semiSep1 Lexer.lexer


commaSep :: (Monad m) => ParsecT Text u m a -> ParsecT Text u m [a]
commaSep = P.commaSep Lexer.lexer


commaSep1 :: (Monad m) => ParsecT Text u m a -> ParsecT Text u m [a]
commaSep1 = P.commaSep1 Lexer.lexer


symbol :: (Monad m) => Text -> ParsecT Text u m Text
symbol s = P.symbol Lexer.lexer (Text.unpack s) <&> Text.pack


parens :: (Monad m) => ParsecT Text u m a -> ParsecT Text u m a
parens = P.parens Lexer.lexer


brackets :: (Monad m) => ParsecT Text u m a -> ParsecT Text u m a
brackets = P.brackets Lexer.lexer

logicValue :: (MonadParse m) => ParsecT Text u m ()
logicValue = void $ try (symbol "true") <|> symbol "false"


label :: (MonadParse m) => ParsecT Text u m ()
label = void identifier


switchIdentifier :: (MonadParse m) => ParsecT Text u m ()
switchIdentifier = void identifier


arrayIdentifier :: (MonadParse m) => ParsecT Text u m ()
arrayIdentifier = void identifier


procedureIdentifier :: (MonadParse m) => ParsecT Text u m ()
procedureIdentifier = void identifier


ifClause :: (MonadParse m) => ParsecT Text u m ()
ifClause = do
  symbol "if" *> booleanExpression *> symbol "then" *> pure ()


----------------------------------------
-- constituents of expressions
----------------------------------------


variable :: (MonadParse m) => ParsecT Text u m ()
variable = do
  try subscriptedVariable
  <|> try functionDesignator
  <|> try simpleVariable


simpleVariable :: (MonadParse m) => ParsecT Text u m ()
simpleVariable = do
  void identifier


subscriptedVariable :: (MonadParse m) => ParsecT Text u m ()
subscriptedVariable = identifier *> brackets subscriptList


subscriptList :: (MonadParse m) => ParsecT Text u m ()
subscriptList = void $ commaSep1 subscriptExpression


subscriptExpression :: (MonadParse m) => ParsecT Text u m ()
subscriptExpression = arithmeticExpression


----------------------------------------
-- expressions
----------------------------------------


expression :: (MonadParse m) => ParsecT Text u m ()
expression = do
  try arithmeticExpression
  <|> try booleanExpression
  <|> try designationalExpression


arithmeticExpression :: (MonadParse m) => ParsecT Text u m ()
arithmeticExpression = do
  _ <- try simpleArithmeticExpression
   <|> try (ifClause
         *> simpleArithmeticExpression
         *> symbol "else"
         *> arithmeticExpression
         *> pure ())
  pure ()


simpleArithmeticExpression :: (MonadParse m) => ParsecT Text u m ()
simpleArithmeticExpression = term `chainl1` addingOperator
  where
    term = try (addingOperator *> term) <|> factor `chainl1` multiplyingOperator
    factor = primary `chainl1` exponentOperator
    primary =
      try (float *> pure ())
      <|> try (integer *> pure ())
      <|> try variable
      <|> parens arithmeticExpression
    addingOperator =
      try (symbol "+" *> pure \_ _ -> ())
      <|> try (symbol "-" *> pure \_ _ -> ())
    multiplyingOperator =
      try (symbol "*" *> pure \_ _ -> ())
      <|> try (symbol "//" *> pure \_ _ -> ())
      <|> try (symbol "/" *> pure \_ _ -> ())
    exponentOperator = try (symbol "**" *> pure \_ _ -> ())


booleanExpression :: (MonadParse m) => ParsecT Text u m ()
booleanExpression = do
  try simpleBooleanExpression
  <|> try (ifClause
        *> simpleBooleanExpression
        *> symbol "else"
        *> booleanExpression
        *> pure ())


simpleBooleanExpression :: (MonadParse m) => ParsecT Text u m ()
simpleBooleanExpression = implication `chainl1` (symbol "equiv" *> pure \_ _ -> ())
  where
    implication = term `chainl1` try (symbol "impl" *> pure \_ _ -> ())
    term = factor `chainl1` try (symbol "and" *> pure \_ _ -> ())
    factor = secondary `chainl1` try (symbol "or" *> pure \_ _ -> ())
    secondary = primary `chainl1` try (symbol "~" *> pure \_ _ -> ())
    primary =
      try logicValue
      <|> try relation
      <|> try variable
      <|> parens booleanExpression
    relation = simpleArithmeticExpression `chainl1` relationalOperator

    relationalOperator = do
      try (symbol "<=" *> pure \_ _ -> ())
       <|> try (symbol "~=" *> pure \_ _ -> ())
       <|> try (symbol ">=" *> pure \_ _ -> ())
       <|> try (symbol ">" *> pure \_ _ -> ())
       <|> try (symbol "<" *> pure \_ _ -> ())
       <|> try (symbol "=") *> pure \_ _ ->( )


designationalExpression :: (MonadParse m) => ParsecT Text u m ()
designationalExpression = do
  try simpleDesignationalExpression
  <|> try (ifClause
        *> simpleDesignationalExpression
        *> symbol "else"
        *> designationalExpression
        *> pure ())


simpleDesignationalExpression :: (MonadParse m) => ParsecT Text u m ()
simpleDesignationalExpression =
  try label
  <|> try switchDesignator
  <|> parens designationalExpression
  where
    switchDesignator = switchIdentifier *> parens subscriptExpression *> pure ()


actualParameter :: (MonadParse m) => ParsecT Text u m ()
actualParameter = do
  try (stringLiteral *> pure ())
  <|> try expression
  <|> try switchIdentifier
  <|> procedureIdentifier


parameterDelimeter :: (MonadParse m) => ParsecT Text u m ()
parameterDelimeter = do
  try (symbol ")" *> many1 (try letter) *> symbol ":(" *> pure ())
  <|> (symbol "," *> pure ())


actualParameterList :: (MonadParse m) => ParsecT Text u m ()
actualParameterList = do
  sepBy1 actualParameter parameterDelimeter  *> pure ()


actualParameterPart :: (MonadParse m) => ParsecT Text u m ()
actualParameterPart = do
  parens actualParameterList *> pure ()


functionDesignator :: (MonadParse m) => ParsecT Text u m ()
functionDesignator = do
  identifier *> actualParameterPart


----------------------------------------
-- declarations
----------------------------------------


declaration :: (MonadParse m) => ParsecT Text u m ()
declaration = do
  try arrayDeclaration
  <|> try typeDeclaration
  <|> try switchDeclaration
  <|> procedureDeclaration


typeList :: (MonadParse m) => ParsecT Text u m ()
typeList = void $ commaSep1 simpleVariable


type_ :: (MonadParse m) => ParsecT Text u m ()
type_ = void $
  try (symbol "real")
  <|> try (symbol "integer")
  <|> symbol "boolean"


localOrOwn :: (MonadParse m) => ParsecT Text u m ()
localOrOwn = void $ optional (symbol "own")


typeDeclaration :: (MonadParse m) => ParsecT Text u m ()
typeDeclaration = localOrOwn *> type_ *> typeList


lowerBound :: (MonadParse m) => ParsecT Text u m ()
lowerBound = arithmeticExpression


upperBound :: (MonadParse m) => ParsecT Text u m ()
upperBound = arithmeticExpression


boundPair :: (MonadParse m) => ParsecT Text u m ()
boundPair = lowerBound *> symbol ":" *> upperBound


boundPairList :: (MonadParse m) => ParsecT Text u m ()
boundPairList = void $ commaSep1 boundPair


arraySegment :: (MonadParse m) => ParsecT Text u m ()
arraySegment = commaSep1 arrayIdentifier *> brackets boundPairList


arrayList :: (MonadParse m) => ParsecT Text u m ()
arrayList = void $ commaSep1 arraySegment


arrayDeclaration :: (MonadParse m) => ParsecT Text u m ()
arrayDeclaration = do
  try (reserved "array" *> arrayList)
  <|> (localOrOwn *> type_ *> reserved "array" *> arrayList)


switchList :: (MonadParse m) => ParsecT Text u m ()
switchList = void $ many1 designationalExpression


switchDeclaration :: (MonadParse m) => ParsecT Text u m ()
switchDeclaration = reserved "switch" *> switchIdentifier *> symbol ":="  *> switchList


formalParameter :: (MonadParse m) => ParsecT Text u m ()
formalParameter = identifier *> pure ()


formalParameterList :: (MonadParse m) => ParsecT Text u m ()
formalParameterList = sepBy1 formalParameter parameterDelimeter  *> pure ()


formalParameterPart :: (MonadParse m) => ParsecT Text u m ()
formalParameterPart = optional (parens formalParameterList) *> pure ()


identifierList :: (MonadParse m) => ParsecT Text u m ()
identifierList = void $ commaSep1 identifier


valuePart :: (MonadParse m) => ParsecT Text u m ()
valuePart = do
  void $ optional (reserved "value" *> identifierList *> symbol ";" *> pure ())


specifier :: (MonadParse m) => ParsecT Text u m ()
specifier = do
  try (reserved "string" *> pure ())
  <|> try (reserved "label" *> pure ())
  <|> try (reserved "switch" *> pure ())
  <|> try (type_ *> reserved "array" *> pure ())
  <|> try (reserved "array" *> pure ())
  <|> try (type_ *> (reserved "procedure" *> pure ()))
  <|> try (reserved "procedure" *> pure ())
  <|> type_


specificationPart :: (MonadParse m) => ParsecT Text u m ()
specificationPart = do
  many (try (specifier *> identifierList *> symbol ";")) *> pure ()


procedureHeading :: (MonadParse m) => ParsecT Text u m ()
procedureHeading = do
  procedureIdentifier
  *> formalParameterPart
  *> symbol ";"
  *> valuePart
  *> specificationPart


procedureBody :: (MonadParse m) => ParsecT Text u m ()
procedureBody = statement


procedureDeclaration :: (MonadParse m) => ParsecT Text u m ()
procedureDeclaration = do
  try (type_ *> reserved "procedure" *> procedureHeading *> procedureBody)
  <|> (reserved "procedure" *> procedureHeading *> procedureBody)


----------------------------------------
-- statements and blocks
----------------------------------------


statement :: (MonadParse m) => ParsecT Text u m ()
statement = do
  try unconditionalStatement <|> try conditionalStatement <|> forStatement


unconditionalStatement :: (MonadParse m) => ParsecT Text u m ()
unconditionalStatement = do
  try basicStatement <|> try compoundStatement <|> block


unlabelledBasicStatement :: (MonadParse m) => ParsecT Text u m ()
unlabelledBasicStatement = do
  try assignmentStatement
  <|> try gotoStatement
  <|> try procedureStatement
  <|> dummyStatement


basicStatement :: (MonadParse m) => ParsecT Text u m ()
basicStatement = do
  try (label *> symbol ":" *> basicStatement) <|> unlabelledBasicStatement


leftPart :: (MonadParse m) => ParsecT Text u m ()
leftPart = do
  try (variable *> symbol ":=" *> pure ())
  <|> try (procedureIdentifier *> symbol ":=" *> pure ())


leftPartList :: (MonadParse m) => ParsecT Text u m ()
leftPartList = do
  many1 (try leftPart) *> pure ()


assignmentStatement :: (MonadParse m) => ParsecT Text u m ()
assignmentStatement = do
  leftPartList *> (try arithmeticExpression <|> booleanExpression)


gotoStatement :: (MonadParse m) => ParsecT Text u m ()
gotoStatement = do
  reserved "goto" *> designationalExpression *> pure ()


procedureStatement :: (MonadParse m) => ParsecT Text u m ()
procedureStatement = do
  try (procedureIdentifier *> actualParameterPart)
  <|> procedureIdentifier


dummyStatement :: (MonadParse m) => ParsecT Text u m ()
dummyStatement =
  try (lookAhead (reserved "end"))
  <|> try (lookAhead (symbol ";" *> pure ()))


forListElement :: (MonadParse m) => ParsecT Text u m ()
forListElement = do
  try (arithmeticExpression *> reserved "step" *> arithmeticExpression *> reserved "until" *> arithmeticExpression)
  <|> try (arithmeticExpression *> reserved "while" *> booleanExpression)
  <|> arithmeticExpression


forList :: (MonadParse m) => ParsecT Text u m ()
forList = commaSep1 forListElement *> pure ()


forClause :: (MonadParse m) => ParsecT Text u m ()
forClause = do
  reserved "for"
  *> variable
  *> symbol ":="
  *> forList
  *> reserved "do"


forStatement :: (MonadParse m) => ParsecT Text u m ()
forStatement = do
  try (label *> symbol ":" *> forStatement) <|> (forClause *> statement)


conditionalStatement :: (MonadParse m) => ParsecT Text u m ()
conditionalStatement = do
  try (label *> symbol ":" *> conditionalStatement)
  <|> conditionalStatement'
  where
    conditionalStatement' = ifClause *>
      (try forStatement
       <|> try (unconditionalStatement *> reserved "else" *> statement)
       <|> unconditionalStatement)


----------------------------------------
-- compound statements and blocks
----------------------------------------


compoundTail :: (MonadParse m) => ParsecT Text u m ()
compoundTail = do
  statement *> (reserved "end" <|> (symbol ";" *> compoundTail))


blockHead :: (MonadParse m) => ParsecT Text u m ()
blockHead = do
  reserved "begin" *> sepEndBy1 declaration (symbol ";") *> pure ()


unlabeledCompoundStatement :: (MonadParse m) => ParsecT Text u m ()
unlabeledCompoundStatement = reserved "begin" *> compoundTail


unlabeledBlock :: (MonadParse m) => ParsecT Text u m ()
unlabeledBlock = do
  blockHead *> compoundTail


compoundStatement :: (MonadParse m) => ParsecT Text u m ()
compoundStatement = do
  try (label *> symbol ":" *> compoundStatement)
  <|> unlabeledCompoundStatement


block :: (MonadParse m) => ParsecT Text u m ()
block = do
  try (label *> symbol ":" *> block) <|> unlabeledBlock


program :: (MonadParse m) => ParsecT Text u m ()
program = try block <|> compoundStatement
