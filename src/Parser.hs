{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

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
  , Procedure (..)
  , Stmt (..)
  , Type (..)
  , Var (..)
  , toBinaryOp
  , toUnaryOp
  )
import Control.Applicative (optional, (<|>))
import Control.Monad
import Control.Monad.State (MonadIO (liftIO))
import Data.Functor (($>))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T
import Lexer qualified
import Position (getPosFromParsec)
import Symbol (Symbol)
import Symbol qualified
import Text.Parsec (ParsecT, anyChar, between, choice, label, many, many1, satisfy, sepBy, sepBy1, sepEndBy, try, parserTrace)
import Text.Parsec.Expr
  ( Assoc (AssocLeft)
  , Operator (Infix, Prefix)
  , buildExpressionParser
  )


-- Algol60 BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm

program :: (MonadIO m) => ParsecT Text u m Stmt
program = do
  parserTrace "<program"
  try block <|> compoundStmt


-------------------------------------------------------------------------------
-- variable
-------------------------------------------------------------------------------
variable :: (MonadIO m) => ParsecT Text u m Var
variable = do
  parserTrace "<variable"
  try subscrptedVariable <|> simpleVariable


simpleVariable :: (MonadIO m) => ParsecT Text u m Var
simpleVariable = do
  parserTrace "simpleVariable"
  SimpleVar <$> identifier <*> getPosFromParsec


subscrptedVariable :: (MonadIO m) => ParsecT Text u m Var
subscrptedVariable = do
  parserTrace "<subscrptedVariable"
  name <- identifier
  pos <- getPosFromParsec
  let var = SimpleVar name pos
  exs <- between (Lexer.symbol "[") (Lexer.symbol "]") (arithmeticExpression `sepBy1` Lexer.reserved ",")
  return $ SubscriptVar var exs pos


-------------------------------------------------------------------------------
-- statement

block :: (MonadIO m) => ParsecT Text u m Stmt
block = do
  parserTrace "<block"
  try unlabelledBlock <|> labelStmt block


unlabelledBlock :: (MonadIO m) => ParsecT Text u m Stmt
unlabelledBlock = do
  parserTrace "unlabelledBlock"
  decs <- blockHead
  case decs of
    [] -> void $ Lexer.symbol ";"
    _ -> pure ()
  statement <- compoundTail
  pos <- getPosFromParsec
  pure . BlockStmt $ BlockStmt_ decs statement pos


blockHead :: (MonadIO m) => ParsecT Text u m [Dec]
blockHead = do
  parserTrace "<blockHead"
  try decl1 <|> decl2
  where
    decl1 = do
      parserTrace "<blockHead.decl1"
      Lexer.reserved "begin"
      comment
      declaration `sepEndBy` Lexer.reserved ";"
    decl2 = do
      parserTrace "blockHead.decl2"
      decs <- blockHead
      Lexer.reserved ";"
      dec <- declaration
      pure (dec : decs)


compoundTail :: (MonadIO m) => ParsecT Text u m Stmt
compoundTail = do
  parserTrace "<compoundTail"
  try stat1 <|> stat2
  where
    stat1 = stmt <* Lexer.reserved "end" <* comment
    stat2 = SeqStmt <$> (stmt <* Lexer.symbol ";") <*> compoundTail


compoundStmt :: (MonadIO m) => ParsecT Text u m Stmt
compoundStmt = do
  parserTrace "<compoundStmt"
  try unlabelledCompound <|> labelStmt compoundStmt


unlabelledCompound :: (MonadIO m) => ParsecT Text u m Stmt
unlabelledCompound = do
  parserTrace "unlabelledCompound"
  Lexer.reserved "begin"
  comment
  compoundTail


comment :: (MonadIO m) => ParsecT Text u m ()
comment = do
  parserTrace "<comment"
  void . optional $ between (Lexer.reserved "comment") (Lexer.reserved ";") (many anyChar)


stmt :: (MonadIO m) => ParsecT Text u m Stmt
stmt = do
  parserTrace "<stmt"
  try forStmt
    <|> try conditionalStmt
    <|> unconditionalStmt


-- conditional
unconditionalStmt :: (MonadIO m) => ParsecT Text u m Stmt
unconditionalStmt = do
  parserTrace "unconditionalStmt"
  try basicStmt <|> try compoundStmt <|> block


conditionalStmt :: (MonadIO m) => ParsecT Text u m Stmt
conditionalStmt = do
  parserTrace "<conditionalStmt"
  ifStmt >>= \case
    IfStmt (IfStmt_ b t NilStmt pos) ->
      do
        try (Lexer.reserved "else" *> stmt)
        <|> try
          ( do
              elseStatement <- stmt
              pure (IfStmt (IfStmt_ b t elseStatement pos))
          )
        <|> labelStmt conditionalStmt
    _ -> fail "invalid conditional"


basicStmt :: (MonadIO m) => ParsecT Text u m Stmt
basicStmt = do
  parserTrace "<basicStmt"
  try unlabelledBasicStmt <|> labelStmt basicStmt


unlabelledBasicStmt :: (MonadIO m) => ParsecT Text u m Stmt
unlabelledBasicStmt = do
  parserTrace "<unlabelledBasicStmt"
  try assignmentStmt <|> try gotoStmt <|> try procedureStmt <|> dummyStmt


ifStmt :: (MonadIO m) => ParsecT Text u m Stmt
ifStmt = do
  parserTrace "<ifStmt"
  ifclause <- ifClauseExpression
  stmt1 <- unconditionalStmt
  stmt2 <- dummyStmt
  pos <- getPosFromParsec
  pure . IfStmt $ IfStmt_ ifclause stmt1 stmt2 pos


-- loop
forStmt :: (MonadIO m) => ParsecT Text u m Stmt
forStmt = do
  parserTrace "<forStmt"
  try unlabelledForClause <|> labelStmt unlabelledForClause


unlabelledForClause :: (MonadIO m) => ParsecT Text u m Stmt
unlabelledForClause = do
  parserTrace "<unlabelledForClause"
  Lexer.reserved "for"
  var <- variable
  Lexer.reserved ":="
  forListEles <- forList
  Lexer.reserved "do"
  body <- stmt
  ForStmt . ForStmt_ var forListEles body <$> getPosFromParsec


forList :: (MonadIO m) => ParsecT Text u m [ForListElement]
forList = do
  parserTrace "<forList"
  flip label "forList" $ forListElement `sepBy1` Lexer.reserved ","


forListElement :: (MonadIO m) => ParsecT Text u m ForListElement
forListElement = do
  parserTrace "<forListElement"
  try step <|> try while <|> arith
 where
  while =
    While
      <$> arithmeticExpression
      <*> (Lexer.reserved "while" *> booleanExpression)
  step =
    Step
      <$> arithmeticExpression
      <*> (Lexer.reserved "step" *> arithmeticExpression)
      <*> (Lexer.reserved "until" *> arithmeticExpression)
  arith = Immediate <$> arithmeticExpression


-- assignment
assignmentStmt :: (MonadIO m) => ParsecT Text u m Stmt
assignmentStmt = do
  parserTrace "<assignmentStmt"
  try assign1 <|> assign2
  where
    assign1 = AssignStmt <$> leftPartList <*> arithmeticExpression <*> getPosFromParsec
    assign2 = AssignStmt <$> leftPartList <*> booleanExpression <*> getPosFromParsec

destination :: (MonadIO m) => ParsecT Text u m Var
destination = variable


leftPart :: (MonadIO m) => ParsecT Text u m Var
leftPart = destination <* Lexer.reserved ":="


leftPartList :: (MonadIO m) => ParsecT Text u m [Var]
leftPartList = many1 leftPart


-- goto
gotoStmt :: (MonadIO m) => ParsecT Text u m Stmt
gotoStmt = do
  parserTrace "<gotStmt"
  Lexer.reserved "goto"
  designationalExpression >>= \case
    LabelExpr sym _ -> pure $ GoToStmt sym
    SwitchExpr sym _ _ -> pure $ GoToStmt sym
    _ -> fail "goto is not followed by a valid desigtional expression"


dummyStmt :: ParsecT Text u m Stmt
dummyStmt = pure NilStmt


-- procedure
procedureStmt :: (MonadIO m) => ParsecT Text u m Stmt
procedureStmt = do
  parserTrace "<procedureStmt"
  CallStmt
    <$> ( CallStmt_
            <$> identifier
            <*> actualParameterPart
            <*> getPosFromParsec)


-------------------------------------------------------------------------------
-- expression

expression :: (MonadIO m) => ParsecT Text u m Expr
expression =
  try ifExpression
    <|> try arithmeticExpression
    <|> try booleanExpression
    <|> designationalExpression


-- binop with position info
binary :: (Monad m) => Text -> Assoc -> Operator Text u m Expr
binary text = Infix $ do
  Lexer.reservedOp text
  position <- getPosFromParsec
  case toBinaryOp text of
    Just op -> pure $ BinopExpr op position
    Nothing -> fail [i|unknown binary operator: #{text}|]


unary :: (Monad m) => Text -> Operator Text u m Expr
unary text = Prefix $ do
  Lexer.reservedOp text
  position <- getPosFromParsec
  case toUnaryOp text of
    Just op -> pure $ UnopExpr op position
    Nothing -> fail [i|unknown binary operator: #{text}|]


varExpression :: (MonadIO m) => ParsecT Text u m Expr
varExpression = VarExpr <$> variable <*> getPosFromParsec


ifClauseExpression :: (MonadIO m) => ParsecT Text u m Expr
ifClauseExpression = Lexer.reserved "if" *> booleanExpression <* Lexer.reserved "then"


ifExpression :: (MonadIO m) => ParsecT Text u m Expr
ifExpression =
  IfExpr
    <$> ifClauseExpression
    <*> (expression <* Lexer.reserved "else")
    <*> expression
    <*> getPosFromParsec


arithmeticExpression :: (MonadIO m) => ParsecT Text u m Expr
arithmeticExpression = simpleArithmeticExpression
 where
  binops =
    [ [binary "*" AssocLeft, binary "/" AssocLeft]
    , [binary "+" AssocLeft, binary "-" AssocLeft]
    ]
  simpleArithmeticExpression = buildExpressionParser binops arithFactor
  arithFactor :: (MonadIO m) => ParsecT Text u m Expr
  arithFactor =
    between (Lexer.symbol "(") (Lexer.symbol ")") arithmeticExpression
      <|> try real
      <|> try int
      <|> try functionDesignator
      <|> varExpression


relationExpression :: (MonadIO m) => ParsecT Text u m Expr
relationExpression = buildExpressionParser relops relationFactor
 where
  relops =
    [
      [ binary ">" AssocLeft
      , binary "<" AssocLeft
      , binary "/=" AssocLeft
      , binary ">=" AssocLeft
      , binary "<=" AssocLeft
      , binary "=" AssocLeft
      ]
    ]

  relationFactor = try arithmeticExpression


booleanExpression :: (MonadIO m) => ParsecT Text u m Expr
booleanExpression = label simpleBooleanExpression "booleanExpression"
 where
  simpleBooleanExpression =
    try $ buildExpressionParser boolops booleanFactor
  boolops =
    [ [unary "not"]
    , [binary "and" AssocLeft]
    , [binary "or" AssocLeft]
    ]
  booleanFactor =
    between (Lexer.symbol "(") (Lexer.symbol ")") booleanExpression
      <|> try relationExpression
      <|> try functionDesignator
      <|> try varExpression
      <|> try logicalValue


labelExpr :: (MonadIO m) => ParsecT Text u m Expr
labelExpr =
  LabelExpr
    <$> (identifier <|> (Lexer.integer >>= (liftIO . Symbol.toSymbol) . T.pack . show))
    <*> getPosFromParsec


labelStmt :: (MonadIO m) => ParsecT Text u m Stmt -> ParsecT Text u m Stmt
labelStmt statement = do
  parserTrace "<labelStmt"
  l <- (try identifier <|> (Lexer.integer >>= (liftIO . Symbol.toSymbol) . T.pack . show)) <* Lexer.symbol ":"
  s <- statement
  pos <- getPosFromParsec
  pure $ LabelStmt l s pos


designationalExpression :: (MonadIO m) => ParsecT Text u m Expr
designationalExpression = simpleDesignationalExpression


simpleDesignationalExpression :: (MonadIO m) => ParsecT Text u m Expr
simpleDesignationalExpression = do
  parserTrace "<simpleDesignationalExpression"
  try (between (Lexer.symbol "(") (Lexer.symbol ")") designationalExpression)
    <|> try switchDesignator
    <|> labelExpr


switchDesignator :: (MonadIO m) => ParsecT Text u m Expr
switchDesignator = do
  parserTrace "<switchDesignator"
  SwitchExpr
    <$> identifier
    <*> between (Lexer.symbol "[") (Lexer.symbol "]") arithmeticExpression
    <*> getPosFromParsec


functionDesignator :: (MonadIO m) => ParsecT Text u m Expr
functionDesignator = do
  parserTrace "<functionDesignator"
  CallExpr
    <$> identifier
    <*> actualParameterPart
    <*> getPosFromParsec


actualParameterPart :: (MonadIO m) => ParsecT Text u m [Expr]
actualParameterPart =
  between (Lexer.symbol "(") (Lexer.symbol ")") actualParameterList


actualParameterList :: (MonadIO m) => ParsecT Text u m [Expr]
actualParameterList = actualParameter `sepBy` Lexer.symbol ","


actualParameter :: (MonadIO m) => ParsecT Text u m Expr
actualParameter = do
  parserTrace "<actualParameter"
  try string
    <|> try varExpression
    <|> expression


identifier :: (MonadIO m) => ParsecT Text u m Symbol
identifier = Lexer.identifier >>= (liftIO . Symbol.toSymbol)


logicalValue :: (Monad m) => ParsecT Text u m Expr
logicalValue = try c1 <|>  c2
  where
    c1 = do
      Lexer.reserved "true"
      IntExpr 1 <$> getPosFromParsec

    c2 = do
      Lexer.reserved "false"
      IntExpr 0 <$> getPosFromParsec


string :: (Monad m) => ParsecT Text u m Expr
string = try s1 <|> s2
  where
    stringLiteral = StringExpr . T.pack <$> many (satisfy (\c -> c `notElem` ['`', '\'', '"'])) <*> getPosFromParsec
    s1 = between (Lexer.symbol "`") (Lexer.symbol "'") stringLiteral
    s2 = between (Lexer.symbol "\"") (Lexer.symbol "\"") stringLiteral


real :: (Monad m) => ParsecT Text u m Expr
real = RealExpr <$> Lexer.float <*> getPosFromParsec


int :: (Monad m) => ParsecT Text u m Expr
int = IntExpr . fromInteger <$> Lexer.integer <*> getPosFromParsec


-------------------------------------------------------------------------------
-- declaration
-------------------------------------------------------------------------------
declaration :: (MonadIO m) => ParsecT Text u m Dec
declaration = do
  parserTrace "<declaration"
  try procedureDeclaration
    <|> try arrayDeclaration
    <|> try switchDeclaration
    <|> typeDeclaration


typeDeclaration :: (MonadIO m) => ParsecT Text u m Dec
typeDeclaration = do
  parserTrace "<typeDeclaration"
  TypeDec <$> localOrOwnType <*> typ <*> typeList <*> getPosFromParsec


typeList :: (MonadIO m) => ParsecT Text u m [Symbol]
typeList = identifier `sepBy` Lexer.symbol ","


typ :: (MonadIO m) => ParsecT Text u m Type
typ = do
  text <- choice [Lexer.symbol "real", Lexer.symbol "integer", Lexer.symbol "boolean"]
  ScalarT <$> liftIO (Symbol.toSymbol (T.pack text))


localOrOwnType :: (MonadIO m) => ParsecT Text u m Bool
localOrOwnType = do
  parserTrace "<localOrOwnType"
  optional (Lexer.reserved "own") >>= \case
    Just () -> pure True
    Nothing -> pure False


arrayDeclaration :: (MonadIO m) => ParsecT Text u m Dec
arrayDeclaration = do
  parserTrace "<arrayDeclaration"
  ArrayDec
    <$> localOrOwnType
    <*> optional typ
    <*> (Lexer.reserved "array" *> arrayList)
    <*> getPosFromParsec


arrayList :: (MonadIO m) => ParsecT Text u m [ArraySegment]
arrayList = arraySegment `sepBy1` Lexer.symbol ","


arraySegment :: (MonadIO m) => ParsecT Text u m ArraySegment
arraySegment = do
  parserTrace "<arraySegment"
  ArraySegment
    <$> (identifier `sepBy1` Lexer.symbol ",")
    <*> between (Lexer.symbol "[") (Lexer.symbol "]") boundPairList


boundPairList :: (MonadIO m) => ParsecT Text u m [(Expr, Expr)]
boundPairList = do
  parserTrace "<boundPairList"
  boundPair `sepBy1` Lexer.symbol ","
 where
  lower = arithmeticExpression
  upper = arithmeticExpression
  boundPair = (,) <$> lower <* Lexer.symbol ":" <*> upper


switchDeclaration :: (MonadIO m) => ParsecT Text u m Dec
switchDeclaration = do
  parserTrace "<switchDeclaration"
  SwitchDec <$> identifier <*> switchList <*> getPosFromParsec


switchList :: (MonadIO m) => ParsecT Text u m [Expr]
switchList = designationalExpression `sepBy1` Lexer.symbol ","


procedureDeclaration :: (MonadIO m) => ParsecT Text u m Dec
procedureDeclaration = do
  parserTrace "<procedureDeclaration"
  returnType <- optional typ
  void $ Lexer.symbol "procedure"
  (procedureIdentifier, parameters) <- procedureHeading
  body <- procedureBody
  position <- getPosFromParsec
  pure
    . ProcedureDec
    $ Procedure
      { name = procedureIdentifier
      , parameters
      , returnType
      , body
      , position
      }


procedureBody :: (MonadIO m) => ParsecT Text u m Stmt
procedureBody = stmt


formalParameterPart :: (MonadIO m) => ParsecT Text u m [Symbol]
formalParameterPart = between (Lexer.symbol "(") (Lexer.symbol ")") identifier `sepBy` Lexer.symbol ","


valuePart :: (MonadIO m) => ParsecT Text u m [Symbol]
valuePart = Lexer.symbol "value" *> (identifier `sepBy1` Lexer.symbol ",") <* Lexer.symbol ";"


specifier :: (MonadIO m) => ParsecT Text u m Type
specifier = do
  parserTrace "<specifier"
  try (Lexer.reserved "string" $> StringT)
    <|> try (Lexer.reserved "label" $> LabelT)
    <|> try (Lexer.reserved "switch" $> SwitchT)
    <|> try (Lexer.reserved "procedure" $> ProcT Nothing)
    <|> try (typ >>= \t -> Lexer.reserved "procedure" $> ProcT (Just t))
    <|> try typ


specificationPart :: (MonadIO m) => ParsecT Text u m [(Symbol, Type)]
specificationPart = do
  parserTrace "<specificationPart"
  tSyms <- ((,) <$> specifier <*> identifier `sepBy1` Lexer.symbol ",") `sepBy` Lexer.symbol ";"
  pure . mconcat . fmap unfold $ tSyms
 where
  unfold (t, syms) = fmap (,t) syms


procedureHeading :: (MonadIO m) => ParsecT Text u m (Symbol, [Parameter])
procedureHeading = do
  parserTrace "<procedureHeading"
  procedureIdentifier <- identifier
  params <- formalParameterPart
  values <- valuePart
  specs <- specificationPart

  unless (Set.fromList (fmap fst specs) `Set.isSubsetOf` Set.fromList params) $ fail "invalid parameter specification"
  unless (Set.fromList values `Set.isSubsetOf` Set.fromList params) $ fail "invalid value specification"

  let specifiedParameters = fmap (toParameter values (Map.fromList specs)) params
  pure
    ( procedureIdentifier
    , specifiedParameters
    )
 where
  toParameter values specs param =
    Paramter
      { name = param
      , typ = Map.lookup param specs
      , evalStrat = if param `elem` values then CBV else CBN
      }
