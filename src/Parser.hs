{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Parser where

import AST
  ( ArraySegment (..)
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
import Text.Parsec (ParsecT, anyChar, between, choice, label, many, many1, manyTill, satisfy, sepBy, sepBy1, sepEndBy, try, (<?>))
import Text.Parsec.Expr
  ( Assoc (AssocLeft)
  , Operator (Infix, Prefix)
  , buildExpressionParser
  )


-- Algol60 BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm

program :: MonadIO m => ParsecT Text u m Stmt
program = try block <|> compoundStmt


-------------------------------------------------------------------------------
-- variable
-------------------------------------------------------------------------------
variable :: MonadIO m => ParsecT Text u m Var
variable = try subscrptedVariable <|> simpleVariable


simpleVariable :: MonadIO m => ParsecT Text u m Var
simpleVariable = SimpleVar <$> identifier <*> getPosFromParsec


subscrptedVariable :: MonadIO m => ParsecT Text u m Var
subscrptedVariable = do
  name <- identifier
  pos <- getPosFromParsec
  let var = SimpleVar name pos
  exs <- between (Lexer.symbol "[") (Lexer.symbol "]") (arithmeticExpression `sepBy1` Lexer.reserved ",")
  return $ SubscriptVar var exs pos


-------------------------------------------------------------------------------
-- statement

block :: MonadIO m => ParsecT Text u m Stmt
block = try unlabelledBlock <|> labelStmt block <?> "block"


unlabelledBlock :: MonadIO m => ParsecT Text u m Stmt
unlabelledBlock = flip label "unlabelled block" $ do
  decs <- blockHead
  case decs of
    [] -> void $ Lexer.symbol ";"
    _ -> pure ()
  statement <- compoundTail
  BlockStmt
    decs
    statement
    <$> getPosFromParsec
    <?> "unlabelled block"


blockHead :: MonadIO m => ParsecT Text u m [Dec]
blockHead = flip label "block head" $ do
  try
    ( do
        Lexer.reserved "begin"
        comment
        declaration `sepEndBy` Lexer.reserved ";"
    )
    <|> ( do
            decs <- blockHead
            Lexer.reserved ";"
            dec <- declaration
            pure (dec : decs)
        )


compoundTail :: MonadIO m => ParsecT Text u m Stmt
compoundTail =
  try (stmt <* Lexer.reserved "end" <* comment)
    <|> (SeqStmt <$> (stmt <* Lexer.symbol ";") <*> compoundTail)


compoundStmt :: MonadIO m => ParsecT Text u m Stmt
compoundStmt = try unlabelledCompound <|> labelStmt compoundStmt <?> "compound stmt"


unlabelledCompound :: MonadIO m => ParsecT Text u m Stmt
unlabelledCompound = flip label "unlabelled compound" $ do
  Lexer.reserved "begin"
  comment
  compoundTail


comment :: MonadIO m => ParsecT Text u m ()
comment = try . void . optional $ do
  Lexer.reserved "comment"
  manyTill anyChar (try (Lexer.symbol ";"))


stmt :: MonadIO m => ParsecT Text u m Stmt
stmt =
  try unconditionalStmt
    <|> try conditionalStmt
    <|> forStmt
      <* comment


-- conditional
unconditionalStmt :: MonadIO m => ParsecT Text u m Stmt
unconditionalStmt = try basicStmt <|> try compoundStmt <|> block


conditionalStmt :: MonadIO m => ParsecT Text u m Stmt
conditionalStmt = do
  ifStmt >>= \case
    IfStmt b t NilStmt pos ->
      do
        try (Lexer.reserved "else" *> stmt)
        <|> try
          ( do
              elseStatement <- stmt
              pure (IfStmt b t elseStatement pos)
          )
        <|> labelStmt conditionalStmt
    _ -> fail "invalid conditional"


basicStmt :: MonadIO m => ParsecT Text u m Stmt
basicStmt = try unlabelledBasicStmt <|> labelStmt basicStmt


unlabelledBasicStmt :: MonadIO m => ParsecT Text u m Stmt
unlabelledBasicStmt = try assignmentStmt <|> try gotoStmt <|> try procedureStmt <|> dummyStmt


ifStmt :: MonadIO m => ParsecT Text u m Stmt
ifStmt = IfStmt <$> ifClauseExpression <*> unconditionalStmt <*> dummyStmt <*> getPosFromParsec <?> "if statment"


-- loop
forStmt :: MonadIO m => ParsecT Text u m Stmt
forStmt = try unlabelledBasicStmt <|> labelStmt unlabelledForClause


unlabelledForClause :: MonadIO m => ParsecT Text u m Stmt
unlabelledForClause = do
  Lexer.reserved "for"
  var <- variable
  Lexer.reserved ":="
  forListEles <- forList
  Lexer.reserved "do"
  body <- stmt
  ForStmt
    var
    forListEles
    body
    <$> getPosFromParsec
    <?> "for"


forList :: MonadIO m => ParsecT Text u m [ForListElement]
forList = forListElement `sepBy1` Lexer.reserved ","


forListElement :: MonadIO m => ParsecT Text u m ForListElement
forListElement = try step <|> try while <|> arith
 where
  while =
    While
      <$> arithmeticExpression
      <*> (Lexer.reserved "while" *> booleanExpression)
      <?> "while"
  step =
    Step
      <$> arithmeticExpression
      <*> (Lexer.reserved "step" *> arithmeticExpression)
      <*> (Lexer.reserved "until" *> arithmeticExpression)
      <?> "step"
  arith = Immediate <$> arithmeticExpression


-- assignment
assignmentStmt :: MonadIO m => ParsecT Text u m Stmt
assignmentStmt =
  try (AssignStmt <$> leftPartList <*> arithmeticExpression <*> getPosFromParsec)
    <|> (AssignStmt <$> leftPartList <*> booleanExpression <*> getPosFromParsec)


destination :: MonadIO m => ParsecT Text u m Var
destination = variable


leftPart :: MonadIO m => ParsecT Text u m Var
leftPart = destination <* Lexer.reserved ":="


leftPartList :: MonadIO m => ParsecT Text u m [Var]
leftPartList = many1 leftPart


-- goto
gotoStmt :: MonadIO m => ParsecT Text u m Stmt
gotoStmt = do
  Lexer.reserved "goto"
  designationalExpression >>= \case
    LabelExpr sym _ -> pure $ GoToStmt sym
    SwitchExpr sym _ _ -> pure $ GoToStmt sym
    _ -> fail "goto is not followed by a valid desigtional expression"


dummyStmt :: ParsecT Text u m Stmt
dummyStmt = pure NilStmt


-- procedure
procedureStmt :: MonadIO m => ParsecT Text u m Stmt
procedureStmt =
  CallStmt
    <$> identifier
    <*> actualParameterPart
    <*> getPosFromParsec
    <?> "function desginator"


-------------------------------------------------------------------------------
-- expression

expression :: MonadIO m => ParsecT Text u m Expr
expression =
  try ifExpression
    <|> try arithmeticExpression
    <|> try booleanExpression
    <|> designationalExpression
    <?> "expression"


-- binop with position info
binary :: Monad m => Text -> Assoc -> Operator Text u m Expr
binary text = Infix $ do
  Lexer.reservedOp text
  position <- getPosFromParsec
  case toBinaryOp text of
    Just op -> pure $ BinopExpr op position
    Nothing -> fail [i|unknown binary operator: #{text}|]


unary :: Monad m => Text -> Operator Text u m Expr
unary text = Prefix $ do
  Lexer.reservedOp text
  position <- getPosFromParsec
  case toUnaryOp text of
    Just op -> pure $ UnopExpr op position
    Nothing -> fail [i|unknown binary operator: #{text}|]


varExpression :: MonadIO m => ParsecT Text u m Expr
varExpression = VarExpr <$> variable <*> getPosFromParsec <?> "variable"


ifClauseExpression :: MonadIO m => ParsecT Text u m Expr
ifClauseExpression = Lexer.reserved "if" *> booleanExpression <* Lexer.reserved "then"


ifExpression :: MonadIO m => ParsecT Text u m Expr
ifExpression =
  IfExpr
    <$> ifClauseExpression
    <*> (expression <* Lexer.reserved "else")
    <*> expression
    <*> getPosFromParsec
    <?> "if"


arithmeticExpression :: MonadIO m => ParsecT Text u m Expr
arithmeticExpression = simpleArithmeticExpression
 where
  binops =
    [ [binary "*" AssocLeft, binary "/" AssocLeft]
    , [binary "+" AssocLeft, binary "-" AssocLeft]
    ]
  simpleArithmeticExpression = buildExpressionParser binops arithFactor
  arithFactor :: MonadIO m => ParsecT Text u m Expr
  arithFactor =
    between (Lexer.symbol "(") (Lexer.symbol ")") arithmeticExpression
      <|> try int
      <|> try real
      <|> try functionDesignator
      <|> varExpression
      <?> "simple expression"


relationExpression :: MonadIO m => ParsecT Text u m Expr
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

  relationFactor = try arithmeticExpression <?> "relation expression"


booleanExpression :: MonadIO m => ParsecT Text u m Expr
booleanExpression = simpleBooleanExpression
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
      <?> "simple expression"


labelExpr :: MonadIO m => ParsecT Text u m Expr
labelExpr =
  LabelExpr
    <$> (identifier <|> (Lexer.integer >>= (liftIO . Symbol.toSymbol) . T.pack . show))
    <*> getPosFromParsec


labelStmt :: MonadIO m => ParsecT Text u m Stmt -> ParsecT Text u m Stmt
labelStmt statement =
  LabelStmt
    <$> ((identifier <|> (Lexer.integer >>= (liftIO . Symbol.toSymbol) . T.pack . show)) <* Lexer.symbol ":")
    <*> statement
    <*> getPosFromParsec
    <?> "label statement"


designationalExpression :: MonadIO m => ParsecT Text u m Expr
designationalExpression = simpleDesignationalExpression


simpleDesignationalExpression :: MonadIO m => ParsecT Text u m Expr
simpleDesignationalExpression =
  try (between (Lexer.symbol "(") (Lexer.symbol ")") designationalExpression)
    <|> try switchDesignator
    <|> labelExpr
    <?> "designational expression"


switchDesignator :: MonadIO m => ParsecT Text u m Expr
switchDesignator =
  SwitchExpr
    <$> identifier
    <*> between (Lexer.symbol "[") (Lexer.symbol "]") arithmeticExpression
    <*> getPosFromParsec
    <?> "switch designator"


functionDesignator :: MonadIO m => ParsecT Text u m Expr
functionDesignator =
  CallExpr
    <$> identifier
    <*> actualParameterPart
    <*> getPosFromParsec
    <?> "function desginator"


actualParameterPart :: MonadIO m => ParsecT Text u m [Expr]
actualParameterPart =
  between (Lexer.symbol "(") (Lexer.symbol ")") actualParameterList
    <?> "actual parameter"


actualParameterList :: MonadIO m => ParsecT Text u m [Expr]
actualParameterList = actualParameter `sepBy` Lexer.symbol ","


actualParameter :: MonadIO m => ParsecT Text u m Expr
actualParameter =
  try string
    <|> try varExpression
    <|> expression
    <?> "actual parameter"


identifier :: MonadIO m => ParsecT Text u m Symbol
identifier = Lexer.identifier >>= (liftIO . Symbol.toSymbol)


logicalValue :: Monad m => ParsecT Text u m Expr
logicalValue = try (Lexer.reserved "true" *> (IntExpr 1 <$> getPosFromParsec)) <|> Lexer.reserved "false" *> (IntExpr 0 <$> getPosFromParsec) <?> "logical value"


string :: Monad m => ParsecT Text u m Expr
string = do
  let stringLiteral = StringExpr . T.pack <$> many (satisfy (\c -> c `notElem` ['`', '\'', '"'])) <*> getPosFromParsec
  try (between (Lexer.symbol "`") (Lexer.symbol "'") stringLiteral)
    <|> between (Lexer.symbol "\"") (Lexer.symbol "\"") stringLiteral
    <?> "string"


real :: Monad m => ParsecT Text u m Expr
real = RealExpr <$> Lexer.float <*> getPosFromParsec <?> "real"


int :: Monad m => ParsecT Text u m Expr
int = IntExpr . fromInteger <$> Lexer.integer <*> getPosFromParsec <?> "int"


-------------------------------------------------------------------------------
-- declaration
-------------------------------------------------------------------------------
declaration :: MonadIO m => ParsecT Text u m Dec
declaration =
  try procedureDeclaration
    <|> try arrayDeclaration
    <|> try switchDeclaration
    <|> typeDeclaration
    <?> "declaration"


typeDeclaration :: MonadIO m => ParsecT Text u m Dec
typeDeclaration = TypeDec <$> localOrOwnType <*> typ <*> typeList <*> getPosFromParsec


typeList :: MonadIO m => ParsecT Text u m [Symbol]
typeList = identifier `sepBy` Lexer.symbol ","


typ :: MonadIO m => ParsecT Text u m Type
typ = do
  text <- choice [Lexer.symbol "real", Lexer.symbol "integer", Lexer.symbol "boolean"]
  ScalarT <$> liftIO (Symbol.toSymbol (T.pack text))


localOrOwnType :: MonadIO m => ParsecT Text u m Bool
localOrOwnType =
  optional (Lexer.reserved "own") >>= \case
    Just () -> pure True
    Nothing -> pure False


-- own real array a1, a2, a3 [1:10],

arrayDeclaration :: MonadIO m => ParsecT Text u m Dec
arrayDeclaration =
  ArrayDec
    <$> localOrOwnType
    <*> optional typ
    <*> (Lexer.reserved "array" *> arrayList)
    <*> getPosFromParsec
    <?> "array declaration"


arrayList :: MonadIO m => ParsecT Text u m [ArraySegment]
arrayList = arraySegment `sepBy1` Lexer.symbol ","


arraySegment :: MonadIO m => ParsecT Text u m ArraySegment
arraySegment =
  ArraySegment
    <$> (identifier `sepBy1` Lexer.symbol ",")
    <*> between (Lexer.symbol "[") (Lexer.symbol "]") boundPairList


boundPairList :: MonadIO m => ParsecT Text u m [(Expr, Expr)]
boundPairList = boundPair `sepBy1` Lexer.symbol ","
 where
  lower = arithmeticExpression
  upper = arithmeticExpression

  boundPair = (,) <$> lower <* Lexer.symbol ":" <*> upper


switchDeclaration :: MonadIO m => ParsecT Text u m Dec
switchDeclaration = SwitchDec <$> identifier <*> switchList <*> getPosFromParsec


switchList :: MonadIO m => ParsecT Text u m [Expr]
switchList = designationalExpression `sepBy1` Lexer.symbol ","


procedureDeclaration :: MonadIO m => ParsecT Text u m Dec
procedureDeclaration = flip label "procedure declaration" $ do
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


procedureBody :: MonadIO m => ParsecT Text u m Stmt
procedureBody = stmt


formalParameterPart :: MonadIO m => ParsecT Text u m [Symbol]
formalParameterPart = between (Lexer.symbol "(") (Lexer.symbol ")") identifier `sepBy` Lexer.symbol ","


valuePart :: MonadIO m => ParsecT Text u m [Symbol]
valuePart = Lexer.symbol "value" *> (identifier `sepBy1` Lexer.symbol ",") <* Lexer.symbol ";"


specifier :: MonadIO m => ParsecT Text u m Type
specifier =
  try (Lexer.reserved "string" $> StringT)
    <|> try (Lexer.reserved "label" $> LabelT)
    <|> try (Lexer.reserved "switch" $> SwitchT)
    <|> try (Lexer.reserved "procedure" $> ProcT Nothing)
    <|> try (typ >>= \t -> Lexer.reserved "procedure" $> ProcT (Just t))
    <|> try typ
    <?> "specifier"


specificationPart :: MonadIO m => ParsecT Text u m [(Symbol, Type)]
specificationPart = do
  tSyms <- ((,) <$> specifier <*> identifier `sepBy1` Lexer.symbol ",") `sepBy` Lexer.symbol ";"
  pure . mconcat . fmap unfold $ tSyms
 where
  unfold (t, syms) = fmap (,t) syms


procedureHeading :: MonadIO m => ParsecT Text u m (Symbol, [Parameter])
procedureHeading = do
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
