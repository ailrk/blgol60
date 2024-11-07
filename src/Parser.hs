{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use module export list" #-}
{-# HLINT ignore "Functor law" #-}

module Parser where

import Data.Text qualified as Text
import Text.Megaparsec (ParsecT, MonadParsec (try), sepEndBy1, lookAhead, sepBy1, between, manyTill, sepBy, choice)
import Text.Megaparsec.Char (letterChar, alphaNumChar, space1, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import AST
import Position (getPosition)
import Symbol (Symbol, toSymbol, HasSymbolTable)
import UnliftIO (MonadUnliftIO)


-- Algol60 BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm
-- http://www.bitsavers.org/pdf/sds/9xx/lang/900699C_Algol60_Ref_Nov66.pdf


----------------------------------------
-- basics
----------------------------------------


reservedWords :: [Text]
reservedWords =
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
  , "equiv"
  , "impl"
  , "step"
  , "while"
  , "comment"
  , "true"
  , "false"
  ]


type Parser m a = ParsecT Void Text m a

type CanParse ctx m = (HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m)

lexeme :: Parser m a -> Parser m a
lexeme = Lexer.lexeme whiteSpace


identifier :: Parser m Text
identifier = (lexeme . try) (p >>= check)
  where
    p = Text.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x =
     if x `elem` reservedWords
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x


-- keywords
reserved :: CanParse ctx m => Text -> Parser m ()
reserved w = symbol w *> pure ()


integer :: Parser m Expr
integer = IntExpr <$> lexeme Lexer.decimal <*> getPosition


float :: Parser m Expr
float = RealExpr <$> lexeme Lexer.float <*> getPosition


-- string literals
stringLiteral :: Parser m Expr
stringLiteral =
  StringExpr
  <$> Text.pack <$> (char '"' >> manyTill Lexer.charLiteral (char '"'))
  <*> getPosition


whiteSpace :: Parser m ()
whiteSpace = Lexer.space space1 empty (Lexer.skipBlockComment "comment" ";")


commaSep :: CanParse ctx m => Parser m a -> Parser m [a]
commaSep p = sepBy p (symbol ",")


commaSep1 :: CanParse ctx m => Parser m a -> Parser m [a]
commaSep1 p = sepBy1 p (symbol ",")


symbol :: CanParse ctx m => Text -> Parser m Symbol
symbol s = Lexer.symbol whiteSpace s >>= lift . toSymbol


parens :: CanParse ctx m => Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")


brackets :: CanParse ctx m => Parser m a -> Parser m a
brackets = between (symbol "[") (symbol "]")


logicValue :: CanParse ctx m => Parser m ()
logicValue = void $ try (symbol "true") <|> symbol "false"


label :: Parser m ()
label = void identifier


switchIdentifier :: Parser m ()
switchIdentifier = void identifier


arrayIdentifier :: Parser m ()
arrayIdentifier = void identifier


procedureIdentifier :: Parser m ()
procedureIdentifier = void identifier


ifClause :: CanParse ctx m => Parser m ()
ifClause = do
  symbol "if" *> booleanExpression *> symbol "then" *> pure ()


----------------------------------------
-- constituents of expressions
----------------------------------------


variable :: CanParse ctx m => Parser m ()
variable = do
  try subscriptedVariable
  <|> try functionDesignator
  <|> try simpleVariable


simpleVariable :: Parser m ()
simpleVariable = do
  void identifier


subscriptedVariable :: CanParse ctx m => Parser m ()
subscriptedVariable = identifier *> brackets subscriptList


subscriptList :: CanParse ctx m => Parser m ()
subscriptList = void $ commaSep1 subscriptExpression


subscriptExpression :: CanParse ctx m => Parser m ()
subscriptExpression = arithmeticExpression


----------------------------------------
-- expressions
----------------------------------------


expression :: CanParse ctx m => Parser m ()
expression = do
  try arithmeticExpression
  <|> try booleanExpression
  <|> try designationalExpression


arithmeticExpression :: CanParse ctx m => Parser m ()
arithmeticExpression = do
  _ <- try simpleArithmeticExpression
   <|> try (ifClause
         *> simpleArithmeticExpression
         *> symbol "else"
         *> arithmeticExpression
         *> pure ())
  pure ()


binary :: CanParse ctx m => Text -> (a -> a -> a) -> Operator (ParsecT Void Text m) a
binary  name f = InfixL  (f <$ symbol name)


binaryR :: CanParse ctx m => Text -> (a -> a -> a) -> Operator (ParsecT Void Text m) a
binaryR  name f = InfixR  (f <$ symbol name)


prefix :: CanParse ctx m => Text -> (a -> a) -> Operator (ParsecT Void Text m) a
prefix  name f = Prefix  (f <$ symbol name)


simpleArithmeticExpression :: CanParse ctx m => Parser m ()
simpleArithmeticExpression = do
  makeExprParser term table
  where
    term = choice
      [ parens arithmeticExpression
      , try $ float *> pure ()
      , try $ integer *> pure ()
      , variable
      ]
    table =
      [ [ binaryR "**" (\_ _ -> ())]
      , [ prefix "+" (const ())
        , prefix "-" (const ())
        ]
      , [ binary "*" (\_ _ -> ())
        , binary "//" (\_ _ -> ())
        , binary "/" (\_ _ -> ())
        ]
      , [ binary "+" (\_ _ -> ())
        , binary "-" (\_ _ -> ())
        ]
      ]


booleanExpression :: CanParse ctx m => Parser m ()
booleanExpression = do
  try simpleBooleanExpression
  <|> try (ifClause
        *> simpleBooleanExpression
        *> symbol "else"
        *> booleanExpression
        *> pure ())


simpleBooleanExpression :: CanParse ctx m => Parser m ()
simpleBooleanExpression = makeExprParser term table
  where
    term =
      try logicValue
      <|> try relation
      <|> try variable
      <|> parens booleanExpression
    relation = makeExprParser simpleArithmeticExpression
      [ [ binary "<=" (\_ _ -> ())
        , binary "~=" (\_ _ -> ())
        , binary ">=" (\_ _ -> ())
        , binary ">" (\_ _ -> ())
        , binary "<" (\_ _ -> ())
        , binary "=" (\_ _ -> ())
        ]
      ]

    table = [ [ prefix "~" (const ()) ]
            , [ binary "or" (\_ _ -> ()) ]
            , [ binary "and" (\_ _ -> ()) ]
            , [ binary "impl" (\_ _ -> ()) ]
            , [ binary "equiv" (\_ _ -> ()) ]
            ]


designationalExpression :: CanParse ctx m => Parser m ()
designationalExpression = do
  try simpleDesignationalExpression
  <|> try (ifClause
        *> simpleDesignationalExpression
        *> symbol "else"
        *> designationalExpression
        *> pure ())


simpleDesignationalExpression :: CanParse ctx m => Parser m ()
simpleDesignationalExpression =
  try label
  <|> try switchDesignator
  <|> parens designationalExpression
  where
    switchDesignator = switchIdentifier *> parens subscriptExpression *> pure ()


actualParameter :: CanParse ctx m => Parser m ()
actualParameter = do
  try (stringLiteral *> pure ())
  <|> try expression
  <|> try switchIdentifier
  <|> procedureIdentifier


parameterDelimeter :: CanParse ctx m => Parser m ()
parameterDelimeter = do
  try (symbol ")" *> some (try letterChar) *> symbol ":(" *> pure ())
  <|> (symbol "," *> pure ())


actualParameterList :: CanParse ctx m => Parser m ()
actualParameterList = do
  sepBy1 actualParameter parameterDelimeter  *> pure ()


actualParameterPart :: CanParse ctx m => Parser m ()
actualParameterPart = do
  parens actualParameterList *> pure ()


functionDesignator :: CanParse ctx m => Parser m ()
functionDesignator = do
  identifier *> actualParameterPart


----------------------------------------
-- declarations
----------------------------------------


declaration :: CanParse ctx m => Parser m ()
declaration = do
  try arrayDeclaration
  <|> try typeDeclaration
  <|> try switchDeclaration
  <|> procedureDeclaration


typeList :: CanParse ctx m => Parser m ()
typeList = void $ commaSep1 simpleVariable


type_ :: CanParse ctx m => Parser m ()
type_ = void $
  try (symbol "real")
  <|> try (symbol "integer")
  <|> symbol "boolean"


localOrOwn :: CanParse ctx m => Parser m ()
localOrOwn = void $ optional (symbol "own")


typeDeclaration :: CanParse ctx m => Parser m ()
typeDeclaration = localOrOwn *> type_ *> typeList


lowerBound :: CanParse ctx m => Parser m ()
lowerBound = arithmeticExpression


upperBound :: CanParse ctx m => Parser m ()
upperBound = arithmeticExpression


boundPair :: CanParse ctx m => Parser m ()
boundPair = lowerBound *> symbol ":" *> upperBound


boundPairList :: CanParse ctx m => Parser m ()
boundPairList = void $ commaSep1 boundPair


arraySegment :: CanParse ctx m => Parser m ()
arraySegment = commaSep1 arrayIdentifier *> brackets boundPairList


arrayList :: CanParse ctx m => Parser m ()
arrayList = void $ commaSep1 arraySegment


arrayDeclaration :: CanParse ctx m => Parser m ()
arrayDeclaration = do
  try (reserved "array" *> arrayList)
  <|> (localOrOwn *> type_ *> reserved "array" *> arrayList)


switchList :: CanParse ctx m => Parser m ()
switchList = some designationalExpression *> pure ()


switchDeclaration :: CanParse ctx m => Parser m ()
switchDeclaration = reserved "switch" *> switchIdentifier *> symbol ":="  *> switchList


formalParameter :: Parser m ()
formalParameter = identifier *> pure ()


formalParameterList :: CanParse ctx m => Parser m ()
formalParameterList = sepBy1 formalParameter parameterDelimeter  *> pure ()


formalParameterPart :: CanParse ctx m => Parser m ()
formalParameterPart = optional (parens formalParameterList) *> pure ()


identifierList :: CanParse ctx m => Parser m ()
identifierList = void $ commaSep1 identifier


valuePart :: CanParse ctx m => Parser m ()
valuePart = do
  void $ optional (reserved "value" *> identifierList *> symbol ";" *> pure ())


specifier :: CanParse ctx m => Parser m ()
specifier = do
  try (reserved "string" *> pure ())
  <|> try (reserved "label" *> pure ())
  <|> try (reserved "switch" *> pure ())
  <|> try (type_ *> reserved "array" *> pure ())
  <|> try (reserved "array" *> pure ())
  <|> try (type_ *> (reserved "procedure" *> pure ()))
  <|> try (reserved "procedure" *> pure ())
  <|> type_


specificationPart :: CanParse ctx m => Parser m ()
specificationPart = do
  many (try (specifier *> identifierList *> symbol ";")) *> pure ()


procedureHeading :: CanParse ctx m => Parser m ()
procedureHeading = do
  procedureIdentifier
  *> formalParameterPart
  *> symbol ";"
  *> valuePart
  *> specificationPart


procedureBody :: CanParse ctx m => Parser m ()
procedureBody = statement


procedureDeclaration :: CanParse ctx m => Parser m ()
procedureDeclaration = do
  try (type_ *> reserved "procedure" *> procedureHeading *> procedureBody)
  <|> (reserved "procedure" *> procedureHeading *> procedureBody)


----------------------------------------
-- statements and blocks
----------------------------------------


statement :: CanParse ctx m => Parser m ()
statement = do
  try unconditionalStatement <|> try conditionalStatement <|> forStatement


unconditionalStatement :: CanParse ctx m => Parser m ()
unconditionalStatement = do
  try basicStatement <|> try compoundStatement <|> block


unlabelledBasicStatement :: CanParse ctx m => Parser m ()
unlabelledBasicStatement = do
  try assignmentStatement
  <|> try gotoStatement
  <|> try procedureStatement
  <|> dummyStatement


basicStatement :: CanParse ctx m => Parser m ()
basicStatement = do
  try (label *> symbol ":" *> basicStatement) <|> unlabelledBasicStatement


leftPart :: CanParse ctx m => Parser m ()
leftPart = do
  try (variable *> symbol ":=" *> pure ())
  <|> try (procedureIdentifier *> symbol ":=" *> pure ())


leftPartList :: CanParse ctx m => Parser m ()
leftPartList = do
  some (try leftPart) *> pure ()


assignmentStatement :: CanParse ctx m => Parser m ()
assignmentStatement = do
  leftPartList *> (try arithmeticExpression <|> booleanExpression)


gotoStatement :: CanParse ctx m => Parser m ()
gotoStatement = do
  reserved "goto" *> designationalExpression *> pure ()


procedureStatement :: CanParse ctx m => Parser m ()
procedureStatement = do
  try (procedureIdentifier *> actualParameterPart)
  <|> procedureIdentifier


dummyStatement :: CanParse ctx m => Parser m ()
dummyStatement =
  try (lookAhead (reserved "end"))
  <|> try (lookAhead (symbol ";" *> pure ()))


forListElement :: CanParse ctx m => Parser m ()
forListElement = do
  try (arithmeticExpression *> reserved "step" *> arithmeticExpression *> reserved "until" *> arithmeticExpression)
  <|> try (arithmeticExpression *> reserved "while" *> booleanExpression)
  <|> arithmeticExpression


forList :: CanParse ctx m => Parser m ()
forList = commaSep1 forListElement *> pure ()


forClause :: CanParse ctx m => Parser m ()
forClause = do
  reserved "for"
  *> variable
  *> symbol ":="
  *> forList
  *> reserved "do"


forStatement :: CanParse ctx m => Parser m ()
forStatement = do
  try (label *> symbol ":" *> forStatement) <|> (forClause *> statement)


conditionalStatement :: CanParse ctx m => Parser m ()
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


compoundTail :: CanParse ctx m => Parser m ()
compoundTail = do
  statement *> (reserved "end" <|> (symbol ";" *> compoundTail))


blockHead :: CanParse ctx m => Parser m ()
blockHead = do
  reserved "begin" *> sepEndBy1 declaration (symbol ";") *> pure ()


unlabeledCompoundStatement :: CanParse ctx m => Parser m ()
unlabeledCompoundStatement = reserved "begin" *> compoundTail


unlabeledBlock :: CanParse ctx m => Parser m ()
unlabeledBlock = do
  blockHead *> compoundTail


compoundStatement :: CanParse ctx m => Parser m ()
compoundStatement = do
  try (label *> symbol ":" *> compoundStatement)
  <|> unlabeledCompoundStatement


block :: CanParse ctx m => Parser m ()
block = do
  try (label *> symbol ":" *> block) <|> unlabeledBlock


program :: CanParse ctx m => Parser m ()
program = try block <|> compoundStatement
