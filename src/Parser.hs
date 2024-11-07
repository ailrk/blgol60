{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser where

import Data.Text qualified as Text
import Text.Megaparsec (ParsecT, MonadParsec (try, getParserState), sepEndBy1, lookAhead, sepBy1, between, manyTill, sepBy, State (..), choice)
import Text.Megaparsec.Char (letterChar, alphaNumChar, space1, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

import Text.Printf

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


lexeme :: ParsecT Void Text m a -> ParsecT Void Text m a
lexeme = Lexer.lexeme whiteSpace


identifier :: ParsecT Void Text m Text
identifier = (lexeme . try) (p >>= check)
  where
    p = Text.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x =
     if x `elem` reservedWords
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x


-- keywords
reserved :: Text -> ParsecT Void Text m ()
reserved w = symbol w *> pure ()


integer :: ParsecT Void Text m Integer
integer = lexeme Lexer.decimal


float :: ParsecT Void Text m Double
float = lexeme Lexer.float


-- string literals
stringLiteral :: ParsecT Void Text m Text
stringLiteral = Text.pack <$> (char '"' >> manyTill Lexer.charLiteral (char '"'))


whiteSpace :: ParsecT Void Text m ()
whiteSpace = Lexer.space space1 empty (Lexer.skipBlockComment "comment" ";")


commaSep :: ParsecT Void Text m a -> ParsecT Void Text m [a]
commaSep p = sepBy p (symbol ",")


commaSep1 :: ParsecT Void Text m a -> ParsecT Void Text m [a]
commaSep1 p = sepBy1 p (symbol ",")


symbol :: Text -> ParsecT Void Text m Text
symbol s = Lexer.symbol whiteSpace s


parens :: ParsecT Void Text m a -> ParsecT Void Text m a
parens = between (symbol "(") (symbol ")")


brackets :: ParsecT Void Text m a -> ParsecT Void Text m a
brackets = between (symbol "[") (symbol "]")


logicValue :: ParsecT Void Text m ()
logicValue = void $ try (symbol "true") <|> symbol "false"


label :: ParsecT Void Text m ()
label = void identifier


switchIdentifier :: ParsecT Void Text m ()
switchIdentifier = void identifier


arrayIdentifier :: ParsecT Void Text m ()
arrayIdentifier = void identifier


procedureIdentifier :: ParsecT Void Text m ()
procedureIdentifier = void identifier


ifClause :: ParsecT Void Text m ()
ifClause = do
  symbol "if" *> booleanExpression *> symbol "then" *> pure ()


----------------------------------------
-- constituents of expressions
----------------------------------------


variable :: ParsecT Void Text m ()
variable = do
  try subscriptedVariable
  <|> try functionDesignator
  <|> try simpleVariable


simpleVariable :: ParsecT Void Text m ()
simpleVariable = do
  void identifier


subscriptedVariable :: ParsecT Void Text m ()
subscriptedVariable = identifier *> brackets subscriptList


subscriptList :: ParsecT Void Text m ()
subscriptList = void $ commaSep1 subscriptExpression


subscriptExpression :: ParsecT Void Text m ()
subscriptExpression = arithmeticExpression


----------------------------------------
-- expressions
----------------------------------------


expression :: ParsecT Void Text m ()
expression = do
  try arithmeticExpression
  <|> try booleanExpression
  <|> try designationalExpression


arithmeticExpression :: ParsecT Void Text m ()
arithmeticExpression = do
  _ <- try simpleArithmeticExpression
   <|> try (ifClause
         *> simpleArithmeticExpression
         *> symbol "else"
         *> arithmeticExpression
         *> pure ())
  pure ()


binary :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text m) a
binary  name f = InfixL  (f <$ symbol name)


binaryR :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text m) a
binaryR  name f = InfixR  (f <$ symbol name)


prefix :: Text -> (a -> a) -> Operator (ParsecT Void Text m) a
prefix  name f = Prefix  (f <$ symbol name)


simpleArithmeticExpression :: ParsecT Void Text m ()
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


booleanExpression :: ParsecT Void Text m ()
booleanExpression = do
  try simpleBooleanExpression
  <|> try (ifClause
        *> simpleBooleanExpression
        *> symbol "else"
        *> booleanExpression
        *> pure ())


simpleBooleanExpression :: ParsecT Void Text m ()
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


designationalExpression :: ParsecT Void Text m ()
designationalExpression = do
  try simpleDesignationalExpression
  <|> try (ifClause
        *> simpleDesignationalExpression
        *> symbol "else"
        *> designationalExpression
        *> pure ())


simpleDesignationalExpression :: ParsecT Void Text m ()
simpleDesignationalExpression =
  try label
  <|> try switchDesignator
  <|> parens designationalExpression
  where
    switchDesignator = switchIdentifier *> parens subscriptExpression *> pure ()


actualParameter :: ParsecT Void Text m ()
actualParameter = do
  try (stringLiteral *> pure ())
  <|> try expression
  <|> try switchIdentifier
  <|> procedureIdentifier


parameterDelimeter :: ParsecT Void Text m ()
parameterDelimeter = do
  try (symbol ")" *> some (try letterChar) *> symbol ":(" *> pure ())
  <|> (symbol "," *> pure ())


actualParameterList :: ParsecT Void Text m ()
actualParameterList = do
  sepBy1 actualParameter parameterDelimeter  *> pure ()


actualParameterPart :: ParsecT Void Text m ()
actualParameterPart = do
  parens actualParameterList *> pure ()


functionDesignator :: ParsecT Void Text m ()
functionDesignator = do
  identifier *> actualParameterPart


----------------------------------------
-- declarations
----------------------------------------


declaration :: ParsecT Void Text m ()
declaration = do
  try arrayDeclaration
  <|> try typeDeclaration
  <|> try switchDeclaration
  <|> procedureDeclaration


typeList :: ParsecT Void Text m ()
typeList = void $ commaSep1 simpleVariable


type_ :: ParsecT Void Text m ()
type_ = void $
  try (symbol "real")
  <|> try (symbol "integer")
  <|> symbol "boolean"


localOrOwn :: ParsecT Void Text m ()
localOrOwn = void $ optional (symbol "own")


typeDeclaration :: ParsecT Void Text m ()
typeDeclaration = localOrOwn *> type_ *> typeList


lowerBound :: ParsecT Void Text m ()
lowerBound = arithmeticExpression


upperBound :: ParsecT Void Text m ()
upperBound = arithmeticExpression


boundPair :: ParsecT Void Text m ()
boundPair = lowerBound *> symbol ":" *> upperBound


boundPairList :: ParsecT Void Text m ()
boundPairList = void $ commaSep1 boundPair


arraySegment :: ParsecT Void Text m ()
arraySegment = commaSep1 arrayIdentifier *> brackets boundPairList


arrayList :: ParsecT Void Text m ()
arrayList = void $ commaSep1 arraySegment


arrayDeclaration :: ParsecT Void Text m ()
arrayDeclaration = do
  try (reserved "array" *> arrayList)
  <|> (localOrOwn *> type_ *> reserved "array" *> arrayList)


switchList :: ParsecT Void Text m ()
switchList = some designationalExpression *> pure ()


switchDeclaration :: ParsecT Void Text m ()
switchDeclaration = reserved "switch" *> switchIdentifier *> symbol ":="  *> switchList


formalParameter :: ParsecT Void Text m ()
formalParameter = identifier *> pure ()


formalParameterList :: ParsecT Void Text m ()
formalParameterList = sepBy1 formalParameter parameterDelimeter  *> pure ()


formalParameterPart :: ParsecT Void Text m ()
formalParameterPart = optional (parens formalParameterList) *> pure ()


identifierList :: ParsecT Void Text m ()
identifierList = void $ commaSep1 identifier


valuePart :: ParsecT Void Text m ()
valuePart = do
  void $ optional (reserved "value" *> identifierList *> symbol ";" *> pure ())


specifier :: ParsecT Void Text m ()
specifier = do
  try (reserved "string" *> pure ())
  <|> try (reserved "label" *> pure ())
  <|> try (reserved "switch" *> pure ())
  <|> try (type_ *> reserved "array" *> pure ())
  <|> try (reserved "array" *> pure ())
  <|> try (type_ *> (reserved "procedure" *> pure ()))
  <|> try (reserved "procedure" *> pure ())
  <|> type_


specificationPart :: ParsecT Void Text m ()
specificationPart = do
  many (try (specifier *> identifierList *> symbol ";")) *> pure ()


procedureHeading :: ParsecT Void Text m ()
procedureHeading = do
  procedureIdentifier
  *> formalParameterPart
  *> symbol ";"
  *> valuePart
  *> specificationPart


procedureBody :: ParsecT Void Text m ()
procedureBody = statement


procedureDeclaration :: ParsecT Void Text m ()
procedureDeclaration = do
  try (type_ *> reserved "procedure" *> procedureHeading *> procedureBody)
  <|> (reserved "procedure" *> procedureHeading *> procedureBody)


----------------------------------------
-- statements and blocks
----------------------------------------


statement :: ParsecT Void Text m ()
statement = do
  try unconditionalStatement <|> try conditionalStatement <|> forStatement


unconditionalStatement :: ParsecT Void Text m ()
unconditionalStatement = do
  try basicStatement <|> try compoundStatement <|> block


unlabelledBasicStatement :: ParsecT Void Text m ()
unlabelledBasicStatement = do
  try assignmentStatement
  <|> try gotoStatement
  <|> try procedureStatement
  <|> dummyStatement


basicStatement :: ParsecT Void Text m ()
basicStatement = do
  try (label *> symbol ":" *> basicStatement) <|> unlabelledBasicStatement


leftPart :: ParsecT Void Text m ()
leftPart = do
  try (variable *> symbol ":=" *> pure ())
  <|> try (procedureIdentifier *> symbol ":=" *> pure ())


leftPartList :: ParsecT Void Text m ()
leftPartList = do
  some (try leftPart) *> pure ()


assignmentStatement :: ParsecT Void Text m ()
assignmentStatement = do
  leftPartList *> (try arithmeticExpression <|> booleanExpression)


gotoStatement :: ParsecT Void Text m ()
gotoStatement = do
  reserved "goto" *> designationalExpression *> pure ()


procedureStatement :: ParsecT Void Text m ()
procedureStatement = do
  try (procedureIdentifier *> actualParameterPart)
  <|> procedureIdentifier


dummyStatement :: ParsecT Void Text m ()
dummyStatement =
  try (lookAhead (reserved "end"))
  <|> try (lookAhead (symbol ";" *> pure ()))


forListElement :: ParsecT Void Text m ()
forListElement = do
  try (arithmeticExpression *> reserved "step" *> arithmeticExpression *> reserved "until" *> arithmeticExpression)
  <|> try (arithmeticExpression *> reserved "while" *> booleanExpression)
  <|> arithmeticExpression


forList :: ParsecT Void Text m ()
forList = commaSep1 forListElement *> pure ()


forClause :: ParsecT Void Text m ()
forClause = do
  reserved "for"
  *> variable
  *> symbol ":="
  *> forList
  *> reserved "do"


forStatement :: ParsecT Void Text m ()
forStatement = do
  try (label *> symbol ":" *> forStatement) <|> (forClause *> statement)


conditionalStatement :: ParsecT Void Text m ()
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


compoundTail :: ParsecT Void Text m ()
compoundTail = do
  statement *> (reserved "end" <|> (symbol ";" *> compoundTail))


blockHead :: ParsecT Void Text m ()
blockHead = do
  reserved "begin" *> sepEndBy1 declaration (symbol ";") *> pure ()


unlabeledCompoundStatement :: ParsecT Void Text m ()
unlabeledCompoundStatement = reserved "begin" *> compoundTail


unlabeledBlock :: ParsecT Void Text m ()
unlabeledBlock = do
  blockHead *> compoundTail


compoundStatement :: ParsecT Void Text m ()
compoundStatement = do
  try (label *> symbol ":" *> compoundStatement)
  <|> unlabeledCompoundStatement


block :: ParsecT Void Text m ()
block = do
  try (label *> symbol ":" *> block) <|> unlabeledBlock


program :: ParsecT Void Text m ()
program = try block <|> compoundStatement


debug :: String -> ParsecT Void Text m ()
debug n = do
  s <- getParserState
  let x = printf "%s>\n%s" n s.stateInput
  traceM x
