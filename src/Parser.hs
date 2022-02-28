-- doge-algol60
-- Copyright © 2021 ailrk
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
-- OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
-- OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Parser where

import qualified AST                   as A
import           Control.Applicative   ((<|>))
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Text             as T
import           Data.Text             (Text)
import           Lexer
import           Position
import           Symbol                as S
import           Text.Parsec           ((<?>))
import qualified Text.Parsec           as P
import qualified Text.Parsec.Expr      as P
import qualified Text.Parsec.Pos       as P
import qualified Text.Parsec.Prim      as P
import qualified Text.Parsec.Text      as P
-- Algol60 BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm

type Parser u a = P.ParsecT T.Text u IO a

toplevelp :: Parser u A.Stmt
toplevelp = blockp <|> compouondStmtp

-------------------------------------------------------------------------------
-- variable
-------------------------------------------------------------------------------
variablep :: Parser u A.Var
variablep = P.try subscrptedVariablep <|> simpleVariablep

simpleVariablep = A.SimpleVar <$> identifierp <*> getPosFromParsec

subscrptedVariablep = do
  name <- identifierp
  pos <- getPosFromParsec
  let var = A.SimpleVar name pos
  exs <- P.between (symbol "[") (symbol "]") (P.many arithmeticExpressionp)
  return $ A.SubscriptVar var exs pos

-------------------------------------------------------------------------------
-- statement
-------------------------------------------------------------------------------
-- block
blockp :: Parser u A.Stmt
blockp = unlabelledBlockp <|> (labelp *> symbol ":" *>  blockp)

-- TODO
blockHeadp = undefined
unlabelledBlockp = blockHeadp *> symbol ":" *> compoundTail

identifiervarp :: Parser u A.Var
identifiervarp = undefined

-- statement
compouondStmtp :: Parser u A.Stmt
compouondStmtp = unlabelledCompoundp
             <|> (labelp *> symbol ":" *> compouondStmtp)

unlabelledCompoundp = reserved "begin" *> compoundTail
compoundTail = undefined

stmtP = undefined
basicStmtp = undefined
unlabelledBasicStmtp = undefined

-- conditional
unconditionalStmtp = undefined
conditionalStmt = undefined

ifStmtp = undefined
ifClausep = undefined

-- loop
forStmtp = undefined
  where
    forClausep = undefined
    forList = undefined
    forListElement = undefined

-- assignment
assignmentStmtp = undefined
  where
    destinationp = undefined

    leftPartp = undefined
    leftPartListp = undefined

-- goto
gotoStmtp = undefined

dummyStmtp = undefined

-- procedure
procedureStmtp = undefined
  where
    actualParameterPartp = undefined
    actualParameterListp = undefined
    parameterDelimiter = undefined

-------------------------------------------------------------------------------
-- expression
-------------------------------------------------------------------------------
expressionp :: Parser u A.Expr
expressionp = ifExpressionp
          <|> P.try designationalExpressionp
          <|> P.try arithmeticExpressionp
          <|> P.try booleanExpressionp

-- binop with position info
binaryp :: [Char] -> P.Assoc -> P.Operator Text u IO A.Expr
binaryp s assoc = P.Infix p assoc
  where
    p = do
      reservedOp s
      pos <- getPosFromParsec
      return (A.BinopExpr (A.toBinaryOp $ T.pack s) pos)

unaryp :: [Char] -> P.Operator Text u IO A.Expr
unaryp s = P.Prefix p
  where
    p = do
      reservedOp s
      pos <- getPosFromParsec
      return (A.UnopExpr (A.toUnaryOp $ T.pack s) pos)

varexprp :: P.ParsecT Text u IO A.Expr
varexprp = A.VarExpr <$> variablep <*> getPosFromParsec

ifclausep :: P.ParsecT Text u IO A.Expr
ifclausep = reserved "if" *> booleanExpressionp <* reserved "then"

ifExpressionp :: Parser u A.Expr
ifExpressionp = A.IfExpr
            <$> ifclausep
            <*> (expressionp <* reserved "else")
            <*> expressionp
            <*> getPosFromParsec

arithmeticExpressionp :: Parser u A.Expr
arithmeticExpressionp = simpleArithmeticExpressionp
  where
    binops = [[ binaryp "*" P.AssocLeft, binaryp "/" P.AssocLeft]
             ,[ binaryp "+" P.AssocLeft, binaryp "-" P.AssocLeft]]
    simpleArithmeticExpressionp = P.buildExpressionParser binops arithFactor
    arithFactor :: Parser u A.Expr
    arithFactor = P.between (symbol "(") (symbol ")") arithmeticExpressionp
         <|> P.try functionDesignatorp
         <|> P.try varexprp
         <|> P.try intp
         <|> P.try realp
         <?> "simple expression"

relationExpressionp = P.buildExpressionParser relops relationFactor
  where
    relops = [[ binaryp ">" P.AssocLeft
              , binaryp "<" P.AssocLeft
              , binaryp "/=" P.AssocLeft
              , binaryp ">=" P.AssocLeft
              , binaryp "<=" P.AssocLeft
              , binaryp "=" P.AssocLeft]]

    relationFactor = P.try arithmeticExpressionp
                 <?> "relation expression"

booleanExpressionp :: Parser u A.Expr
booleanExpressionp = simpleBooleanExpressionp
  where
    simpleBooleanExpressionp
      = P.try $ P.buildExpressionParser boolops booleanFactor
    boolops = [[ unaryp "not"]
              ,[ binaryp "and" P.AssocLeft]
              ,[ binaryp "or" P.AssocLeft]]
    booleanFactor = P.between (symbol "(") (symbol ")") booleanExpressionp
                <|> P.try relationExpressionp
                <|> P.try functionDesignatorp
                <|> P.try varexprp
                <|> P.try logicalValuep
                <?> "simple expression"

labelp :: Parser u A.Expr
labelp = A.LabelExpr
     <$> (identifierp <|> (T.pack . show <$> integer >>= liftIO . toSymbol))
     <*> getPosFromParsec

designationalExpressionp :: Parser u A.Expr
designationalExpressionp = simpleDesignationalExpressionp

simpleDesignationalExpressionp
  = P.between (symbol "(") (symbol ")") designationalExpressionp
 <|> P.try switchDesignatorp
 <|> P.try labelp

switchDesignatorp = A.SwitchExpr
                <$> identifierp
                <*> P.between (symbol "[") (symbol "]") arithmeticExpressionp
                <*> getPosFromParsec
                <?> "switch designator"

functionDesignatorp = A.CallExpr
                  <$> identifierp
                  <*> P.option [] actualParameterPartp
                  <*> getPosFromParsec
                  <?> "function desginator"

actualParameterPartp = P.between (symbol "(") (symbol ")") acutalParameterListp

acutalParameterListp = P.sepBy actualParameterp (symbol ",")

actualParameterp = P.try stringp
              <|> P.try varexprp
              <|> P.try expressionp
              <?> "actual parameter"

identifierp :: Parser u Symbol
identifierp = (T.pack <$> identifier) >>= (liftIO . toSymbol)

logicalValuep :: Parser u A.Expr
logicalValuep = P.try (reserved "true" *> (A.IntExpr 1 <$> getPosFromParsec))
            <|> reserved "false" *> (A.IntExpr 0 <$> getPosFromParsec)
            <?> "logical value"

stringp :: Parser u A.Expr
stringp = symbol "`"
       *> (A.StringExpr . T.pack
          <$> (P.many $ P.satisfy (\c -> not $ c `elem` ['`', '\'']))
          <*> getPosFromParsec)
       <* symbol "'"
       <?> "string"

realp :: Parser u A.Expr
realp = A.RealExpr <$> float <*> getPosFromParsec <?> "real"

intp :: Parser u A.Expr
intp = (A.IntExpr . fromInteger) <$> integer <*>  getPosFromParsec <?> "int"

-------------------------------------------------------------------------------
-- declaration
-------------------------------------------------------------------------------
declarationp :: Parser u A.Dec
declarationp = undefined
  where
    typeDeclarationp :: Parser u A.Dec
    typeDeclarationp = undefined

    arrayDeclarationp :: Parser u A.Dec
    arrayDeclarationp = undefined

    switchDeclarationp :: Parser u A.Dec
    switchDeclarationp = undefined

    procedureDeclarationp :: Parser u A.Dec
    procedureDeclarationp = undefined
