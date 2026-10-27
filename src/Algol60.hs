{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RoleAnnotations #-}
-- Requires GHC >= 9.2 (NoFieldSelectors).
module Main (main) where

import Data.Hashable qualified as Hashable
import Data.Map qualified as Map
import Text.Megaparsec
  ( Pos, TraversableStream, ParsecT, SourcePos (..), getSourcePos
  , (<|>), many, some, optional, runParserT, errorBundlePretty )
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (Map)
import UnliftIO (MonadIO, IORef, readIORef, writeIORef, newIORef, MonadUnliftIO, modifyIORef, liftIO)
import Control.Monad.Reader (MonadReader, asks, ReaderT, MonadTrans (..), runReaderT)
import Control.DeepSeq (NFData)
import Control.Monad.Combinators (skipMany)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Text.Megaparsec
  ( MonadParsec (try), sepEndBy1, lookAhead, sepBy1, between, manyTill, sepBy
  , choice, anySingle, satisfy, (<?>), notFollowedBy, eof, takeWhile1P )
import Text.Megaparsec.Char (letterChar, alphaNumChar, space1, char, space, newline, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Void (Void)
import Control.Lens ((<&>))
import Data.Maybe (isJust, maybeToList, fromMaybe)
import Control.Monad (join, void, foldM, forM, when, zipWithM)
import Data.Bifunctor (Bifunctor(..))
import Data.List (find)
import Data.Char (isAlphaNum)
import System.Environment (getArgs)
import System.Exit (exitFailure)


------------------------------------------------------------
-- # AST
------------------------------------------------------------


data Var
  = SimpleVar
      { name     :: Symbol
      , position :: Position
      }
  | SubscriptVar
      { name     :: Symbol
      , subs     :: [Expr]
      , position :: Position
      }
  | FunctionDesignator
      { name     :: Symbol
      , params   :: [Expr]
      , position :: Position
      }
  deriving (Show, Read, Eq)


-- Algo60 only have Int, real, and string three primitive types.
data Expr
  = NilExpr
  | VarExpr Var
  | IntExpr Integer Position
  | RealExpr Double Position
  | BoolExpr Bool Position
  | StringExpr Text Position
  | LabelExpr
      { name     :: Symbol
      , position :: Position
      }
  | SwitchExpr
      { name     :: Symbol
      , subs     :: [Expr]
      , position :: Position
      }
  | CallExpr
      { name     :: Symbol
      , args     :: [Expr]
      , position :: Position
      }
  | IfExpr
      { test     :: Expr
      , then_    :: Expr
      , else_    :: Expr
      , position :: Position
      }
  | BinopExpr
      { binop    :: BinaryOp
      , position :: Position
      , left     :: Expr
      , right    :: Expr
      }
  | UnopExpr
      { unop     :: UnaryOp
      , position :: Position
      , operand  :: Expr
      }
  deriving (Show, Read, Eq)


data Stmt
  = BlockStmt
      { decs :: [Dec]
      , body :: Stmt
      , position :: Position
      }
  | CallStmt
      { name     :: Symbol
      , args     :: [Expr]
      , position :: Position
      }
  | LabelStmt Symbol Stmt
  | VarStmt Var
  | SeqStmt Stmt Stmt
  | AssignStmt
      { lhs      :: [Var]
      , rhs      :: Expr
      , position :: Position
      }
  | IfStmt
      { test     :: Expr
      , then_    :: Stmt
      , else_    :: Stmt
      , position :: Position
      }
  | ForStmt
      { var             :: Var
      , forListElements :: [ForListElement]
      -- ^ for list elements, in SOURCE order.
      --
      --  e.g if the statement is `for 1, 2, step 1 until 4, 1, 2`
      --  the list will be [1, 2, step 1 until 4, 1, 2]
      , body            :: Stmt
      , position        :: Position
      }
  | GotoStmt Expr
  | DummyStmt
  deriving (Show, Read, Eq)


data ForListElement
  = While
      Expr -- arithmetic expr
      Expr -- test
  | Step
      Expr -- init arithmetic expr
      Expr -- step
      Expr -- until
  | Immediate Expr
  deriving (Show, Read, Eq)


data Dec
  = ProcedureDec
      { name       :: Symbol
      , params     :: [Param]
      , returnType :: Maybe Type
      , body       :: Stmt
      , position   :: Position
      }
  | TypeDec
      { own         :: Bool
      , typeDecList :: Type
      , nameList    :: [Var]
      , position    :: Position
      }
  | ArrayDec
      { own       :: Bool
      , arrayType :: Maybe Type
      , segments  :: [ArraySegment]
      , position  :: Position
      }
  | SwitchDec
      { name               :: Symbol
      , designationalExprs :: [Expr]
      , position           :: Position
      }
  deriving (Show, Read, Eq)


data ArraySegment = ArraySegment
  { names  :: [Symbol]
  , bounds :: [(Expr, Expr)]
  }
  deriving (Show, Read, Eq, Generic)


-- type annotations
data Type
  = IntegerT
  | RealT
  | BooleanT
  | ArrayT
  | UnknownT
  deriving (Show, Read, Eq)

instance Semigroup Type where
  a <> _ = a


-- OPerators
data BinaryOp
  = PlusOp | MinusOp | TimesOp | DivOp | IntDivOp | ExpOp | EqOp | NeqOp | LtOp
  | LeOp | GtOp | GeOp | AndOp | OrOp | EquivOp | ImplOp
  deriving (Eq, Show, Read)


data UnaryOp = NotOp | BitNotOp | PositiveOp | NegativeOp deriving (Eq, Show, Read)


data Param = Param
  { name      :: Symbol
  , typ       :: Type
  , evalStrat :: EvalStrat
  }
  deriving (Show, Read, Eq, Generic)


data EvalStrat = CBV | CBN deriving (Show, Read, Eq)


------------------------------------------------------------
-- # Parser
------------------------------------------------------------

-- CompilerM BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm
-- http://www.bitsavers.org/pdf/sds/9xx/lang/900699C_CompilerM_Ref_Nov66.pdf


reservedWords :: [Text]
reservedWords =
  [ "begin", "end", "own", "real", "integer", "boolean", "array", "switch", "procedure"
  , "value", "string", "label", "goto", "if", "then", "else", "do", "for", "or", "and"
  , "not", "equiv", "impl", "step", "until", "while", "comment", "true", "false"
  ]


type Parser a =  ParsecT Void Text CompilerM a


lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme whiteSpace


identifier :: Parser Symbol
identifier = (lexeme . try) (p >>= check) >>= lift . toSymbol
  where
    p = Text.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    check x =
     if x `elem` reservedWords
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x


-- | A keyword: must not be followed by an identifier character, so that
-- `end` does not match the prefix of `endx` and `not` the prefix of `nothing`.
keyword :: Text -> Parser ()
keyword w = lexeme (try (string w *> notFollowedBy (alphaNumChar <|> char '_')))


reserved :: Text -> Parser ()
reserved = keyword


integer :: Parser Expr
integer = IntExpr <$> lexeme Lexer.decimal <*> getPosition


float :: Parser Expr
float = RealExpr <$> lexeme Lexer.float <*> getPosition


-- string literals
stringLiteral :: Parser Expr
stringLiteral = do
  s <- lexeme (char '"' *> manyTill Lexer.charLiteral (char '"'))
  StringExpr (Text.pack s) <$> getPosition


whiteSpace :: Parser ()
whiteSpace = Lexer.space space1 mempty (Lexer.skipBlockComment "comment" ";")


-- look ahead without consuming. Useful for debugging
currentChar :: Parser Char
currentChar = lookAhead anySingle


commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")


commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p (symbol ",")


-- | Punctuation / operators.  Use `keyword` for words.
symbol :: Text -> Parser Symbol
symbol s = Lexer.symbol whiteSpace s >>= lift . toSymbol


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")


logicValue :: Parser Expr
logicValue = BoolExpr <$> c <*> getPosition
  where
    c = (keyword "true" *> pure True) <|> (keyword "false" *> pure False)


label :: Parser Symbol
label = identifier


switchIdentifier :: Parser Symbol
switchIdentifier = identifier


arrayIdentifier :: Parser Symbol
arrayIdentifier = identifier


procedureIdentifier :: Parser Symbol
procedureIdentifier = identifier


ifClause :: Parser Expr
ifClause = keyword "if" *> booleanExpression <* keyword "then"


variable :: Parser Var
variable = do
  try subscriptedVariable
  <|> try functionDesignator
  <|> try simpleVariable


simpleVariable :: Parser Var
simpleVariable =
  SimpleVar <$> identifier <*> getPosition


subscriptedVariable :: Parser Var
subscriptedVariable =
  SubscriptVar <$> identifier <*> brackets subscriptList <*> getPosition


subscriptList :: Parser [Expr]
subscriptList = commaSep1 subscriptExpression


subscriptExpression :: Parser Expr
subscriptExpression = arithmeticExpression


expression :: Parser Expr
expression = do
  try booleanExpression
  <|> try arithmeticExpression
  <|> designationalExpression
  <?> "expression"


arithmeticExpression :: Parser Expr
arithmeticExpression =
  try simpleArithmeticExpression <|> do
    pos <- getPosition
    c   <- ifClause
    a   <- simpleArithmeticExpression
    reserved "else"
    b   <- arithmeticExpression
    pure (IfExpr c a b pos)


binary :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text CompilerM) a
binary  name f = InfixL (f <$ symbol name)


binaryR :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text CompilerM) a
binaryR  name f = InfixR (f <$ symbol name)


prefix :: Text -> (a -> a) -> Operator (ParsecT Void Text CompilerM) a
prefix  name f = Prefix (f <$ symbol name)


-- keyword versions (word-boundary aware)
binaryK :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text CompilerM) a
binaryK name f = InfixL (f <$ keyword name)


prefixK :: Text -> (a -> a) -> Operator (ParsecT Void Text CompilerM) a
prefixK name f = Prefix (f <$ keyword name)


simpleArithmeticExpression :: Parser Expr
simpleArithmeticExpression = do
  pos <- getPosition
  makeExprParser term
      [ [ binaryR "^" (BinopExpr ExpOp pos)]
      , [ prefix "+" (UnopExpr PositiveOp pos)
        , prefix "-" (UnopExpr NegativeOp pos)
        ]
      , [ binary "*" (BinopExpr TimesOp pos)
        , binary "//" (BinopExpr IntDivOp pos)
        , binary "/" (BinopExpr DivOp pos)
        ]
      , [ binary "+" (BinopExpr PlusOp pos)
        , binary "-" (BinopExpr MinusOp pos)
        ]
      ]
  where
    term = choice
      [ parens arithmeticExpression
      , try float
      , try integer
      , VarExpr <$> variable
      ]


booleanExpression :: Parser Expr
booleanExpression =
  try simpleBooleanExpression <|> do
    pos <- getPosition
    c   <- ifClause
    a   <- simpleBooleanExpression
    reserved "else"
    b   <- booleanExpression
    pure (IfExpr c a b pos)


-- Algol 60 precedence (high to low): not, and, or, impl, equiv
simpleBooleanExpression :: Parser Expr
simpleBooleanExpression = do
  pos <- getPosition
  makeExprParser term
    [ [ prefix "~" (UnopExpr NotOp pos), prefixK "not" (UnopExpr NotOp pos) ]
    , [ binaryK "and" (BinopExpr AndOp pos) ]
    , [ binaryK "or" (BinopExpr OrOp pos) ]
    , [ binaryK "impl" (BinopExpr ImplOp pos) ]
    , [ binaryK "equiv" (BinopExpr EquivOp pos) ]
    ]
  where
    term =
      try logicValue
      <|> try relation
      <|> try (VarExpr <$> variable)
      <|> parens booleanExpression
    relation = do
      pos <- getPosition
      makeExprParser simpleArithmeticExpression
        [ [ binary "<=" (BinopExpr LeOp pos)
          , binary "~=" (BinopExpr NeqOp pos)
          , binary ">=" (BinopExpr GeOp pos)
          , binary ">" (BinopExpr GtOp pos)
          , binary "<" (BinopExpr LtOp pos)
          , binary "=" (BinopExpr EqOp pos)
          ]
        ]


designationalExpression :: Parser Expr
designationalExpression =
  try simpleDesignationalExpression <|> do
    pos <- getPosition
    c   <- ifClause
    a   <- simpleDesignationalExpression
    reserved "else"
    b   <- designationalExpression
    pure (IfExpr c a b pos)


simpleDesignationalExpression :: Parser Expr
simpleDesignationalExpression =
  try (SwitchExpr <$> switchIdentifier <*> (pure <$> brackets subscriptExpression) <*> getPosition)
  <|> try (LabelExpr <$> label <*> getPosition)
  <|> parens designationalExpression


actualParameter :: Parser Expr
actualParameter = do
  try stringLiteral
  <|> try expression


parameterDelimeter :: Parser ()
parameterDelimeter = do
  try (symbol ")" *> some (try letterChar) *> symbol ":(" *> pure ())
  <|> (symbol "," *> pure ())


actualParameterList :: Parser [Expr]
actualParameterList = do
  sepBy1 actualParameter parameterDelimeter


actualParameterPart :: Parser [Expr]
actualParameterPart = do
  parens actualParameterList


functionDesignator :: Parser Var
functionDesignator =
  FunctionDesignator <$> identifier <*> actualParameterPart <*> getPosition


declaration :: Parser Dec
declaration = do
  try arrayDeclaration
  <|> try typeDeclaration
  <|> try switchDeclaration
  <|> procedureDeclaration
  <?> "declaration"


typeList :: Parser [Var]
typeList = commaSep1 simpleVariable


type_ :: Parser Type
type_ =
  try (keyword "real" *> pure RealT)
  <|> try (keyword "integer" *> pure IntegerT)
  <|> (keyword "boolean" *> pure BooleanT)


localOrOwn :: Parser Bool
localOrOwn = optional (keyword "own") <&> isJust


typeDeclaration :: Parser Dec
typeDeclaration = TypeDec <$> localOrOwn <*> type_ <*> typeList <*> getPosition


lowerBound :: Parser Expr
lowerBound = arithmeticExpression


upperBound :: Parser Expr
upperBound = arithmeticExpression


boundPair :: Parser (Expr, Expr)
boundPair = (,) <$> lowerBound <* symbol ":" <*> upperBound


boundPairList :: Parser [(Expr, Expr)]
boundPairList = commaSep1 boundPair


arraySegment :: Parser ArraySegment
arraySegment = ArraySegment <$> commaSep1 arrayIdentifier <*> brackets boundPairList


arrayList :: Parser [ArraySegment]
arrayList = commaSep1 arraySegment


arrayDeclaration :: Parser Dec
arrayDeclaration = try simple <|> full
  where
    simple =
      reserved "array" *>
      (ArrayDec False Nothing <$> arrayList <*> getPosition)
    full =
      ArrayDec
      <$> localOrOwn
      <*> (pure <$> type_)
      <*> (reserved "array" *> arrayList)
      <*> getPosition


switchList :: Parser [Expr]
switchList = commaSep1 designationalExpression


switchDeclaration :: Parser Dec
switchDeclaration =
  reserved "switch" *>
    (SwitchDec <$> switchIdentifier <* symbol ":="  <*> switchList <*> getPosition)


formalParameter :: Parser Symbol
formalParameter = identifier


formalParameterList :: Parser [Symbol]
formalParameterList = sepBy1 formalParameter parameterDelimeter


formalParameterPart :: Parser [Symbol]
formalParameterPart = optional (parens formalParameterList) <&> join . maybeToList


identifierList :: Parser [Symbol]
identifierList = commaSep1 identifier


valuePart :: Parser [Symbol]
valuePart = optional (reserved "value" *> identifierList <* symbol ";") <&> join . maybeToList


specifier :: Parser (Maybe Type)
specifier = do
  try (reserved "string" *> pure Nothing)
  <|> try (reserved "label" *> pure Nothing)
  <|> try (reserved "switch" *> pure Nothing)
  <|> try (type_ <* reserved "array" <&> pure)
  <|> try (reserved "array" *> pure Nothing)
  <|> try (type_ <* reserved "procedure" <&> pure)
  <|> try (reserved "procedure" *> pure Nothing)
  <|> (type_ <&> pure)


specificationPart :: Parser [(Maybe Type, [Symbol])]
specificationPart = do
  many (try ((,) <$> specifier <*> identifierList <* symbol ";"))


procedure :: Parser Dec
procedure = do
  name       <- procedureIdentifier
  formals    <- formalParameterPart <* symbol ";"
  values     <- Set.fromList <$> valuePart
  specifiers <- fmap (second Set.fromList) <$> specificationPart
  let params = formals <&> \n ->
        Param n
          (let p = Set.member n . snd
               mtype = find p specifiers >>= fst
            in fromMaybe UnknownT mtype)
          (if Set.member n values then CBV else CBN)
  body <- procedureBody
  pos  <- getPosition
  pure
    ProcedureDec
      { name = name
      , params = params
      , returnType = Nothing
      , body = body
      , position = pos
      }


procedureBody :: Parser Stmt
procedureBody = statement


procedureDeclaration :: Parser Dec
procedureDeclaration = do
  mType <- try (Just <$> type_) <|> pure Nothing
  reserved "procedure"
  dec <- procedure
  pure dec { returnType = mType }


statement :: Parser Stmt
statement =
  try conditionalStatement
  <|> try unconditionalStatement
  <|> forStatement


unconditionalStatement :: Parser Stmt
unconditionalStatement = do
  try basicStatement <|> try compoundStatement <|> block


unlabelledBasicStatement :: Parser Stmt
unlabelledBasicStatement = do
  try assignmentStatement
  <|> try gotoStatement
  <|> try procedureStatement
  <|> dummyStatement


basicStatement :: Parser Stmt
basicStatement = do
  try (LabelStmt <$> label <* symbol ":" <*> basicStatement)
  <|> unlabelledBasicStatement


leftPart :: Parser Var
leftPart = do
  try (variable <* symbol ":=")


leftPartList :: Parser [Var]
leftPartList = do
  some (try leftPart)


assignmentStatement :: Parser Stmt
assignmentStatement =
  AssignStmt
  <$> leftPartList
  <*> (try booleanExpression <|> arithmeticExpression)
  <*> getPosition


gotoStatement :: Parser Stmt
gotoStatement = GotoStmt <$> (reserved "goto" *> designationalExpression)


procedureStatement :: Parser Stmt
procedureStatement =
  try (CallStmt <$> procedureIdentifier <*> actualParameterPart <*> getPosition)
  <|> (CallStmt <$> procedureIdentifier <*> pure [] <*> getPosition)


dummyStatement :: Parser Stmt
dummyStatement = do
  try (lookAhead (reserved "end"))
    <|> try (lookAhead (reserved "else"))
    <|> try (lookAhead (symbol ";" *> pure ()))
  pure DummyStmt


forListElement :: Parser ForListElement
forListElement = do
  try (Step <$> arithmeticExpression <* reserved "step" <*> arithmeticExpression <* reserved "until" <*> arithmeticExpression)
  <|> try (While <$> arithmeticExpression <* reserved "while" <*> booleanExpression)
  <|> (Immediate <$> arithmeticExpression)


forList :: Parser [ForListElement]
forList = commaSep1 forListElement


forStatement :: Parser Stmt
forStatement =
  try (LabelStmt <$> label <* symbol ":" <*> forStatement)
  <|> (forClause <*> statement <*> getPosition)
  where
    forClause = do
      reserved "for"
      name <- variable
      _ <- symbol ":="
      forListElements  <- forList
      reserved "do"
      pure (ForStmt name forListElements)


conditionalStatement :: Parser Stmt
conditionalStatement =
  try (LabelStmt <$> label <* symbol ":" <*> conditionalStatement)
  <|> do
    c   <- ifClause
    pos <- getPosition
    th  <- try forStatement <|> unconditionalStatement
    el  <- (reserved "else" *> statement) <|> pure DummyStmt
    pure (IfStmt c th el pos)


compoundTail :: Parser Stmt
compoundTail = SeqStmt <$> statement <*> next <?> "compoundTail"
  where
    -- text between `end` and the next `;`, `end` or `else` is a comment
    endComment :: Parser ()
    endComment = void (many chunk)

    chunk :: Parser ()
    chunk =
      try wordChunk
      <|> try (void (satisfy (\c -> c /= ';' && not (isAlphaNum c || c == '_'))))

    wordChunk :: Parser ()
    wordChunk = do
      w <- takeWhile1P Nothing (\c -> isAlphaNum c || c == '_')
      if w `elem` ["end", "else"] then fail "stop" else pure ()

    next =
      reserved "end" *> endComment *> pure DummyStmt
      <|> symbol ";" *> compoundTail


blockHead :: Parser [Dec]
blockHead = do
  reserved "begin" *> sepEndBy1 declaration (symbol ";")


unlabeledCompoundStatement :: Parser Stmt
unlabeledCompoundStatement = reserved "begin" *> compoundTail


unlabeledBlock :: Parser Stmt
unlabeledBlock = do
  BlockStmt <$> blockHead <*> compoundTail <*> getPosition


compoundStatement :: Parser Stmt
compoundStatement = do
  try (LabelStmt <$> label <* symbol ":" <*> compoundStatement)
  <|> unlabeledCompoundStatement
  <?> "compoundStatement"


block :: Parser Stmt
block = try (LabelStmt <$> label <* symbol ":" <*> block) <|> unlabeledBlock <?> "block"


program :: Parser Stmt
program = do
  emptyLines
  stmt <- try block <|> compoundStatement
  emptyLines
  eof
  pure stmt
  where
    emptyLines = skipMany (void space1 <|> void (newline *> space))


------------------------------------------------------------
-- # Tree
------------------------------------------------------------

data STMT
  = MOVE EXPR EXPR                 -- MOVE (MEM addr) value
  | EXP EXPR
  | CJUMP RELOP EXPR EXPR Label Label
  | JUMP Label
  | SEQ STMT STMT
  | LABEL Label


data EXPR
  = CONST Int
  | NAME Label                     -- address of a function
  | STRLIT Label Text              -- address of a string constant
  | TEMP Temp                      -- only the frame pointer (Temp 0) for now
  | BINOP BINOP EXPR EXPR
  | MEM EXPR
  | CALL EXPR [EXPR]               -- Algol-to-Algol call (args on stack)
  | CALLC Label [EXPR]             -- call into the C runtime
  | ESEQ STMT EXPR


data BINOP = PLUS | MINUS | MUL | DIV | AND | OR | XOR
-- constructors renamed: EQ/LT/GT clash with Prelude's Ordering
data RELOP = REQ | RNE | RLT | RGT | RLE | RGE


seqs :: [STMT] -> STMT
seqs [] = EXP (CONST 0)
seqs xs = foldr1 SEQ xs


-- | A compiled function body, ready for instruction emission.
data Fragment = Fragment
  { fragLabel :: Label
  , fragFrame :: Frame
  , fragBody  :: STMT
  , fragRet   :: Maybe EXPR   -- value to leave in rax
  }


------------------------------------------------------------
-- # Symbols
------------------------------------------------------------


newtype SymbolTable = SymbolTable (Map Text Int)


data Symbol = Symbol
  { text :: Text
  , hash :: Int
  }
  deriving (Show, Read, Eq, Ord)


newSymbolTable :: SymbolTable
newSymbolTable = SymbolTable Map.empty


toSymbol :: (MonadReader Context m, MonadIO m) => Text -> m Symbol
toSymbol text = do
  symbolTable <- asks (.symbolTableRef) >>= readIORef
  case lookupSymbol text symbolTable of
    Just s -> pure s
    Nothing -> do
      let n = Hashable.hash text
      ref <- asks (.symbolTableRef)
      ref `writeIORef` insertSymbol text n symbolTable
      pure (Symbol text n)


lookupSymbol :: Text -> SymbolTable -> Maybe Symbol
lookupSymbol text (SymbolTable m) =
  case Map.lookup text m of
    Just n -> Just (Symbol text n)
    Nothing -> Nothing


insertSymbol :: Text -> Int -> SymbolTable -> SymbolTable
insertSymbol text hash (SymbolTable m) = SymbolTable (Map.insert text hash m)


data Position = Position
  { file :: FilePath
  , col :: !Pos
  , line :: !Pos
  }
  deriving (Eq, Show, Read, Generic, NFData)


getPosition :: (TraversableStream s, Ord e) => ParsecT e s m Position
getPosition = do
  SourcePos {..} <- getSourcePos
  pure Position
    { file = sourceName
    , line = sourceLine
    , col = sourceColumn
    }


------------------------------------------------------------
-- # Temp / Label
------------------------------------------------------------


-- | Represents a code/data label
type Label = Symbol

-- | Represent a virtual register that holds a temporary value.
data Temp = Temp Int (Maybe Text.Text) deriving (Show, Eq)


-- unique label
newNamedLabel :: (MonadReader Context m, MonadIO m) => Text -> m Label
newNamedLabel name = do
  ref <- asks (.labelCounter)
  modifyIORef ref (+ 1)
  i <- readIORef ref
  toSymbol (Text.concat [name, Text.pack (show i)])


newLabel :: (MonadReader Context m, MonadIO m) => m Label
newLabel = newNamedLabel "l"


-- unique temporary (Temp 0 is reserved for the frame pointer)
newNamedTemp :: (MonadReader Context m, MonadIO m) => Text -> m Temp
newNamedTemp name = do
  ref <- asks (.tempCounter)
  modifyIORef ref (+ 1)
  i <- readIORef ref
  pure (Temp i (Just name))


newTemp :: (MonadReader Context m, MonadIO m) => m Temp
newTemp = newNamedTemp "t"


------------------------------------------------------------
-- # Context
------------------------------------------------------------


data Context = Context
  { symbolTableRef :: IORef SymbolTable
  , tempCounter    :: IORef Int
  , labelCounter   :: IORef Int
  , fragmentsRef   :: IORef [Fragment]
  }


newContext :: MonadIO m => m Context
newContext = do
  symbolTableRef <- newIORef newSymbolTable
  tempCounter    <- newIORef 0
  labelCounter   <- newIORef 0
  fragmentsRef   <- newIORef []
  pure Context {..}


------------------------------------------------------------
-- # Semantic: frames, levels, environments
------------------------------------------------------------
--
-- Stack frame layout (x86-64, after `push rbp; mov rbp, rsp`):
--     [rbp+8]    return address
--     [rbp+16]   static link (frame pointer of the declaring scope)
--     [rbp+24].. parameters (CBV = 1 word, CBN = 2 words: thunk code, thunk fp)
--     [rbp-8]    saved rbx
--     [rbp-16].. locals / temporaries
--
-- Call-by-name: each actual becomes a "thunk" function that returns the
-- ADDRESS of the value (so Jensen's device and assignment to name parameters
-- work).  Non-lvalue actuals are computed into a scratch slot of the creating
-- frame and that slot's address is returned.

data Access
  = InReg Temp
  | InFrame Int                    -- byte offset from rbp
  deriving (Show, Eq)


data Level = Level
  { parent :: Maybe Level
  , frame  :: Frame
  , depth  :: Int
  }


data EnvEntry
  = VarEntry
      { access :: Access
      , ty     :: Type
      , level  :: Level            -- level whose frame holds the variable
      , strat  :: EvalStrat
      }
  | ArrEntry
      { offsets :: [Int]           -- [ptr, lo1, ext1, lo2, ext2, ...]
      , dims    :: Int
      , level   :: Level
      }
  | ProcEntry
      { procLabel :: Label
      , level     :: Level         -- the procedure's OWN level
      , params    :: [Param]
      , retTy     :: Maybe Type
      }


data Frame = Frame
  { name        :: Label
  , formals     :: [Access]        -- first word of each (non static-link) param
  , localsCount :: IORef Int       -- slot 0 is reserved for saved rbx
  }


type Env = Map Symbol EnvEntry


-- | Translation environment.
data TEnv = TEnv
  { vars   :: Env
  , cur    :: Level
  , result :: Maybe (Symbol, Level, Int)
    -- ^ inside a typed procedure: its name and the slot holding its result
  }


word :: Int
word = 8


fpTemp :: Temp
fpTemp = Temp 0 (Just "fp")


fp :: EXPR
fp = TEMP fpTemp


paramWords :: Param -> Int
paramWords p = case p.evalStrat of
  CBV -> 1
  CBN -> 2


newFrame :: Label -> [Param] -> CompilerM Frame
newFrame lbl ps = do
  ref <- newIORef 1
  let idxs = scanl (+) 1 (map paramWords ps)
      fs   = [InFrame (16 + word * k) | k <- take (length ps) idxs]
  pure Frame { name = lbl, formals = fs, localsCount = ref }


-- | Level for a procedure declared inside `par`.
newProcLevel :: Level -> Label -> [Param] -> CompilerM Level
newProcLevel par lbl ps = do
  fr <- newFrame lbl ps
  pure Level { parent = Just par, frame = fr, depth = par.depth + 1 }


allocLocal :: Frame -> CompilerM Int
allocLocal fr = do
  k <- readIORef fr.localsCount
  writeIORef fr.localsCount (k + 1)
  pure (negate (word * (k + 1)))


allocLocals :: Frame -> Int -> CompilerM [Int]
allocLocals fr n = mapM (const (allocLocal fr)) [1 .. n]


-- | Frame pointer of `dest`, starting from `current`: one hop follows the
-- static link stored at [fp+16].
followLinks :: Level -> Level -> EXPR -> EXPR
followLinks current dest base
  | current.depth == dest.depth = base
  | otherwise =
      case current.parent of
        Just p  -> followLinks p dest (MEM (BINOP PLUS base (CONST 16)))
        Nothing -> error "Scope error: destination level is not an ancestor"


slotAddr :: Level -> Level -> Int -> EXPR
slotAddr current dest off = BINOP PLUS (followLinks current dest fp) (CONST off)


-- | A slot in the *current* frame (as an lvalue/rvalue MEM node).
cell :: Int -> EXPR
cell off = MEM (BINOP PLUS fp (CONST off))


unsupported :: String -> a
unsupported what = error ("not supported yet: " ++ what)


addFragment :: Fragment -> CompilerM ()
addFragment f = do
  ref <- asks (.fragmentsRef)
  modifyIORef ref (f :)


accessOff :: Access -> Int
accessOff (InFrame o) = o
accessOff (InReg _)   = error "register-allocated variables not used yet"


userLabel :: TEnv -> Symbol -> CompilerM Label
userLabel tenv n = toSymbol (Text.concat [tenv.cur.frame.name.text, "_u_", n.text])


------------------------------------------------------------
-- # Translation: expressions
------------------------------------------------------------

relOp :: BinaryOp -> Maybe RELOP
relOp op = case op of
  EqOp  -> Just REQ
  NeqOp -> Just RNE
  LtOp  -> Just RLT
  LeOp  -> Just RLE
  GtOp  -> Just RGT
  GeOp  -> Just RGE
  _     -> Nothing


transExpr :: TEnv -> Expr -> CompilerM EXPR
transExpr tenv expr =
  case expr of
    NilExpr           -> pure (CONST 0)
    IntExpr n _       -> pure (CONST (fromInteger n))
    BoolExpr b _      -> pure (CONST (if b then 1 else 0))
    RealExpr _ pos    -> unsupported ("real numbers at " ++ show pos)
    StringExpr s _    -> do l <- newLabel
                            pure (STRLIT l s)
    VarExpr v         -> transVarRead tenv v
    LabelExpr {}      -> unsupported "label as value"
    SwitchExpr {}     -> unsupported "switch designator as value"
    CallExpr { name = n, args = xs, position = pos } -> callProc tenv n pos xs
    IfExpr { test = c, then_ = a, else_ = b } -> do
      t <- newLabel
      f <- newLabel
      j <- newLabel
      off <- allocLocal tenv.cur.frame
      c' <- transCond tenv c t f
      a' <- transExpr tenv a
      b' <- transExpr tenv b
      pure $ ESEQ
        (seqs [ c', LABEL t, MOVE (cell off) a', JUMP j
              , LABEL f, MOVE (cell off) b', LABEL j ])
        (cell off)
    UnopExpr op _ e ->
      case op of
        PositiveOp -> transExpr tenv e
        NegativeOp -> BINOP MINUS (CONST 0) <$> transExpr tenv e
        _          -> boolValue tenv expr
    BinopExpr op _ l r ->
      let arith o = BINOP o <$> transExpr tenv l <*> transExpr tenv r
       in case op of
            PlusOp   -> arith PLUS
            MinusOp  -> arith MINUS
            TimesOp  -> arith MUL
            DivOp    -> arith DIV   -- TODO: Algol '/' always yields a real
            IntDivOp -> arith DIV
            ExpOp    -> do f <- toSymbol "a60_ipow"
                           CALLC f <$> mapM (transExpr tenv) [l, r]
            _        -> boolValue tenv expr


-- | Materialise a boolean expression as 0/1.
boolValue :: TEnv -> Expr -> CompilerM EXPR
boolValue tenv e = do
  t <- newLabel
  f <- newLabel
  j <- newLabel
  off <- allocLocal tenv.cur.frame
  c <- transCond tenv e t f
  pure $ ESEQ
    (seqs [ c, LABEL t, MOVE (cell off) (CONST 1), JUMP j
          , LABEL f, MOVE (cell off) (CONST 0), LABEL j ])
    (cell off)


-- | Translate a boolean expression into control flow (short-circuit).
transCond :: TEnv -> Expr -> Label -> Label -> CompilerM STMT
transCond tenv e t f =
  case e of
    BoolExpr b _          -> pure (JUMP (if b then t else f))
    UnopExpr NotOp _ x    -> transCond tenv x f t
    UnopExpr BitNotOp _ x -> transCond tenv x f t
    BinopExpr op _ l r ->
      case op of
        AndOp -> do
          m <- newLabel
          a <- transCond tenv l m f
          b <- transCond tenv r t f
          pure (seqs [a, LABEL m, b])
        OrOp -> do
          m <- newLabel
          a <- transCond tenv l t m
          b <- transCond tenv r t f
          pure (seqs [a, LABEL m, b])
        ImplOp -> do                       -- a impl b  ==  not a or b
          m <- newLabel
          a <- transCond tenv l m t
          b <- transCond tenv r t f
          pure (seqs [a, LABEL m, b])
        EquivOp -> do
          x <- transExpr tenv l
          y <- transExpr tenv r
          pure (CJUMP REQ x y t f)
        _ | Just rel <- relOp op -> do
              x <- transExpr tenv l
              y <- transExpr tenv r
              pure (CJUMP rel x y t f)
          | otherwise -> generic
    _ -> generic
  where
    generic = do
      v <- transExpr tenv e
      pure (CJUMP RNE v (CONST 0) t f)


transVarRead :: TEnv -> Var -> CompilerM EXPR
transVarRead tenv v =
  case v of
    SimpleVar n pos ->
      case Map.lookup n tenv.vars of
        Just ProcEntry {} -> callProc tenv n pos []
        Just _            -> MEM <$> varAddr tenv v
        Nothing           -> pure (CALLC n [])     -- runtime procedure, no args
    SubscriptVar {}              -> MEM <$> varAddr tenv v
    FunctionDesignator n xs pos  -> callProc tenv n pos xs


-- | Address of an assignable variable (honours "f := value" inside f).
lvalueAddr :: TEnv -> Var -> CompilerM EXPR
lvalueAddr tenv v =
  case v of
    SimpleVar n _
      | Just (rn, rl, off) <- tenv.result, rn == n ->
          pure (slotAddr tenv.cur rl off)
    _ -> varAddr tenv v


varAddr :: TEnv -> Var -> CompilerM EXPR
varAddr tenv v =
  case v of
    SimpleVar n pos ->
      case Map.lookup n tenv.vars of
        Just (VarEntry acc _ lvl st) ->
          let off = accessOff acc
           in pure $ case st of
                CBV -> slotAddr tenv.cur lvl off
                CBN -> CALL (MEM (slotAddr tenv.cur lvl off))
                            [MEM (slotAddr tenv.cur lvl (off + word))]
        Just ArrEntry {}  -> error ("array used without subscripts at " ++ show pos)
        Just ProcEntry {} -> error ("procedure used as variable at " ++ show pos)
        Nothing           -> error ("Undefined variable at " ++ show pos)
    SubscriptVar n subs pos ->
      case Map.lookup n tenv.vars of
        Just (ArrEntry offs k lvl) -> do
          when (length subs /= k) $
            error ("wrong number of subscripts at " ++ show pos)
          idxs <- mapM (transExpr tenv) subs
          let slot o = MEM (slotAddr tenv.cur lvl o)
              (ptrO, rest) = case offs of
                               (p : r) -> (p, r)
                               []      -> error "corrupt array entry"
              pairs = pairUp rest
              step acc (i, (lo, ext)) =
                BINOP PLUS (BINOP MUL acc (slot ext)) (BINOP MINUS i (slot lo))
              ix = case zip idxs pairs of
                     ((i0, (lo0, _)) : more) ->
                       foldl step (BINOP MINUS i0 (slot lo0)) more
                     [] -> error "empty subscript list"
          pure (BINOP PLUS (slot ptrO) (BINOP MUL ix (CONST word)))
        _ -> error ("not an array: " ++ show n.text ++ " at " ++ show pos)
    FunctionDesignator {} -> error "function designator is not assignable"
  where
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _           = []


------------------------------------------------------------
-- # Translation: calls and thunks
------------------------------------------------------------

callProc :: TEnv -> Symbol -> Position -> [Expr] -> CompilerM EXPR
callProc tenv n pos xs =
  case Map.lookup n tenv.vars of
    Just (ProcEntry lbl lvl ps _) -> do
      when (length ps /= length xs) $
        error ("wrong number of arguments to " ++ show n.text ++ " at " ++ show pos)
      declLvl <- case lvl.parent of
                   Just p  -> pure p
                   Nothing -> error "procedure without parent level"
      argEs <- concat <$> zipWithM (transArg tenv) ps xs
      let sl = followLinks tenv.cur declLvl fp
      pure (CALL (NAME lbl) (sl : argEs))
    Just _  -> error (show n.text ++ " is not a procedure (" ++ show pos ++ ")")
    Nothing -> CALLC n <$> mapM (transExpr tenv) xs   -- C runtime procedure


transArg :: TEnv -> Param -> Expr -> CompilerM [EXPR]
transArg tenv p a =
  case p.evalStrat of
    CBV -> (: []) <$> transExpr tenv a
    CBN -> do th <- mkThunk tenv a
              pure [th, fp]


lvalueVar :: TEnv -> Expr -> Maybe Var
lvalueVar tenv (VarExpr v@(SimpleVar n _))
  | Just VarEntry {} <- Map.lookup n tenv.vars = Just v
lvalueVar _ (VarExpr v@SubscriptVar {}) = Just v
lvalueVar _ _ = Nothing


-- | Build a thunk function for call-by-name; returns its code address.
-- The thunk receives the creator's frame pointer as its static link and
-- returns the address of the value.
mkThunk :: TEnv -> Expr -> CompilerM EXPR
mkThunk tenv actual = do
  lbl <- newNamedLabel "a60_thunk_"
  lvl <- newProcLevel tenv.cur lbl []
  let tenv' = tenv { cur = lvl, result = Nothing }
  retE <-
    case lvalueVar tenv actual of
      Just v  -> varAddr tenv' v
      Nothing -> do
        e   <- transExpr tenv' actual
        off <- allocLocal tenv.cur.frame      -- scratch slot in creator frame
        let addr = slotAddr lvl tenv.cur off
        pure (ESEQ (MOVE (MEM addr) e) addr)
  addFragment (Fragment lbl lvl.frame (EXP (CONST 0)) (Just retE))
  pure (NAME lbl)


------------------------------------------------------------
-- # Translation: declarations
------------------------------------------------------------

transDecs :: TEnv -> [Dec] -> CompilerM (TEnv, STMT)
transDecs tenv0 decs = do
  -- 1. register procedures (mutual recursion, use-before-declare)
  tenv1 <- foldM declareProc tenv0 decs
  -- 2. allocate variables and arrays, collect their initialisation code
  (tenv2, inits) <-
    foldM (\(env, acc) d -> do
              (env', ss) <- declareVars env d
              pure (env', acc ++ ss))
          (tenv1, []) decs
  -- 3. compile procedure bodies with the complete environment
  mapM_ (compileProc tenv2) decs
  pure (tenv2, seqs inits)


declareProc :: TEnv -> Dec -> CompilerM TEnv
declareProc tenv d =
  case d of
    ProcedureDec { name = n, params = ps, returnType = rt } -> do
      lbl <- newNamedLabel (Text.concat ["a60_", n.text, "_"])
      lvl <- newProcLevel tenv.cur lbl ps
      pure tenv { vars = Map.insert n (ProcEntry lbl lvl ps rt) tenv.vars }
    _ -> pure tenv


declareVars :: TEnv -> Dec -> CompilerM (TEnv, [STMT])
declareVars tenv d =
  case d of
    -- TODO: `own` should live in .data, not in the frame
    TypeDec { typeDecList = t, nameList = vs } -> do
      when (t == RealT) $ unsupported "real variables"
      foldM (\(env, acc) v ->
               case v of
                 SimpleVar n _ -> do
                   off <- allocLocal env.cur.frame
                   let entry = VarEntry (InFrame off) t env.cur CBV
                   pure ( env { vars = Map.insert n entry env.vars }
                        , acc ++ [MOVE (cell off) (CONST 0)] )
                 _ -> error "bad variable in type declaration")
            (tenv, []) vs

    ArrayDec { segments = segs } ->
      foldM (\acc0 (ArraySegment ns bs) ->
               foldM (declareArray bs) acc0 ns)
            (tenv, []) segs

    SwitchDec {}    -> unsupported "switch declarations"
    ProcedureDec {} -> pure (tenv, [])


declareArray :: [(Expr, Expr)] -> (TEnv, [STMT]) -> Symbol -> CompilerM (TEnv, [STMT])
declareArray bs (env, acc) n = do
  offs <- allocLocals env.cur.frame (1 + 2 * length bs)
  let (ptrO, rest) = case offs of
                       (p : r) -> (p, r)
                       []      -> error "impossible"
      pairs = pairUp rest
  boundCode <-
    forM (zip bs pairs) $ \((lo, hi), (loO, extO)) -> do
      lo' <- transExpr env lo
      hi' <- transExpr env hi
      pure [ MOVE (cell loO) lo'
           , MOVE (cell extO) (BINOP PLUS (BINOP MINUS hi' (cell loO)) (CONST 1)) ]
  allocFn <- toSymbol "a60_alloc"
  let total = foldr1 (BINOP MUL) [cell e | (_, e) <- pairs]
      alloc = MOVE (cell ptrO) (CALLC allocFn [BINOP MUL total (CONST word)])
      entry = ArrEntry { offsets = offs, dims = length bs, level = env.cur }
  pure ( env { vars = Map.insert n entry env.vars }
       , acc ++ concat boundCode ++ [alloc] )
  where
    pairUp (a : b : r) = (a, b) : pairUp r
    pairUp _           = []


compileProc :: TEnv -> Dec -> CompilerM ()
compileProc tenv d =
  case d of
    ProcedureDec { name = n, returnType = rt, body = b } ->
      case Map.lookup n tenv.vars of
        Just (ProcEntry lbl lvl ps _) -> do
          let bind :: TEnv -> (Param, Access) -> TEnv
              bind env (p, acc) =
                env { vars = Map.insert p.name
                               (VarEntry acc p.typ lvl p.evalStrat) env.vars }
              base = foldl bind (tenv { cur = lvl, result = Nothing })
                           (zip ps lvl.frame.formals)
          (res, retE) <-
            case rt of
              Nothing -> pure (Nothing, Nothing)
              Just _  -> do
                off <- allocLocal lvl.frame
                pure ( Just (n, lvl, off)
                     , Just (MEM (BINOP PLUS fp (CONST off))) )
          body' <- transStmt (base { result = res }) b
          addFragment (Fragment lbl lvl.frame body' retE)
        _ -> error "procedure missing from environment"
    _ -> pure ()


------------------------------------------------------------
-- # Translation: statements
------------------------------------------------------------

transStmt :: TEnv -> Stmt -> CompilerM STMT
transStmt tenv stmt =
  case stmt of
    BlockStmt { decs = ds, body = b } -> do
      (tenv', inits) <- transDecs tenv ds
      b' <- transStmt tenv' b
      pure (SEQ inits b')

    CallStmt { name = n, args = xs, position = pos } ->
      EXP <$> callProc tenv n pos xs

    LabelStmt l s -> do
      lbl <- userLabel tenv l
      SEQ (LABEL lbl) <$> transStmt tenv s

    VarStmt v -> EXP <$> transVarRead tenv v

    SeqStmt a b -> SEQ <$> transStmt tenv a <*> transStmt tenv b

    AssignStmt { lhs = vs, rhs = r } -> do
      e <- transExpr tenv r
      case vs of
        [v] -> do a <- lvalueAddr tenv v
                  pure (MOVE (MEM a) e)
        _   -> do off   <- allocLocal tenv.cur.frame     -- a := b := e
                  addrs <- mapM (lvalueAddr tenv) vs
                  pure (seqs (MOVE (cell off) e : [MOVE (MEM a) (cell off) | a <- addrs]))

    IfStmt { test = c, then_ = a, else_ = b } -> do
      t <- newLabel
      f <- newLabel
      j <- newLabel
      c' <- transCond tenv c t f
      a' <- transStmt tenv a
      b' <- transStmt tenv b
      pure (seqs [c', LABEL t, a', JUMP j, LABEL f, b', LABEL j])

    ForStmt { var = v, forListElements = els, body = b } ->
      seqs <$> mapM (forElem tenv v b) els

    GotoStmt e ->
      case e of
        LabelExpr { name = n } -> JUMP <$> userLabel tenv n
        _ -> unsupported "computed / switch goto"

    DummyStmt -> pure (EXP (CONST 0))


forElem :: TEnv -> Var -> Stmt -> ForListElement -> CompilerM STMT
forElem tenv v b el =
  case el of
    Immediate e -> do
      a  <- lvalueAddr tenv v
      x  <- transExpr tenv e
      b' <- transStmt tenv b
      pure (seqs [MOVE (MEM a) x, b'])

    While e c -> do
      top <- newLabel
      ok  <- newLabel
      out <- newLabel
      a   <- lvalueAddr tenv v
      x   <- transExpr tenv e
      c'  <- transCond tenv c ok out
      b'  <- transStmt tenv b
      pure (seqs [ LABEL top, MOVE (MEM a) x, c', LABEL ok, b'
                 , JUMP top, LABEL out ])

    Step i s u -> do
      top  <- newLabel
      posT <- newLabel
      negT <- newLabel
      bodL <- newLabel
      out  <- newLabel
      a    <- lvalueAddr tenv v
      i'   <- transExpr tenv i
      s'   <- transExpr tenv s
      u'   <- transExpr tenv u
      b'   <- transStmt tenv b
      let var' = MEM a
      pure (seqs
        [ MOVE (MEM a) i'
        , LABEL top
        , CJUMP RGE s' (CONST 0) posT negT
        , LABEL posT, CJUMP RGT var' u' out bodL
        , LABEL negT, CJUMP RLT var' u' out bodL
        , LABEL bodL, b'
        , MOVE (MEM a) (BINOP PLUS var' s')
        , JUMP top
        , LABEL out ])


------------------------------------------------------------
-- # Code generation (Tree -> x86-64, Intel syntax)
------------------------------------------------------------
-- Stack-machine style: every EXPR leaves its value in rax.

ins :: Text -> Text
ins = ("    " <>)


tshow :: Show a => a -> Text
tshow = Text.pack . show


codeLbl :: Label -> Text
codeLbl l = ".L" <> l.text


escapeAsm :: Text -> Text
escapeAsm = Text.concatMap f
  where
    f '"'  = "\\\""
    f '\\' = "\\\\"
    f '\n' = "\\n"
    f c    = Text.singleton c


argRegs :: [Text]
argRegs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]


genExpr :: EXPR -> [Text]
genExpr e =
  case e of
    CONST n -> [ins ("mov rax, " <> tshow n)]
    NAME l  -> [ins ("lea rax, [rip + " <> l.text <> "]")]
    STRLIT l s ->
      [ ins ".section .rodata"
      , codeLbl l <> ":"
      , ins (".asciz \"" <> escapeAsm s <> "\"")
      , ins ".text"
      , ins ("lea rax, [rip + " <> codeLbl l <> "]") ]
    TEMP (Temp 0 _) -> [ins "mov rax, rbp"]
    TEMP t -> error ("unexpected temporary " ++ show t)
    BINOP op a b ->
      genExpr b ++ [ins "push rax"] ++ genExpr a ++ [ins "pop rcx"] ++ genBinop op
    MEM a -> genExpr a ++ [ins "mov rax, [rax]"]
    CALL f args ->
      let pushes = concatMap (\a -> genExpr a ++ [ins "push rax"]) (reverse args)
          target = case f of
                     NAME l -> [ins ("call " <> l.text)]
                     _      -> genExpr f ++ [ins "call rax"]
       in pushes ++ target ++ [ins ("add rsp, " <> tshow (word * length args))]
    CALLC l args
      | length args > 6 -> error "too many arguments to runtime call"
      | otherwise ->
          let n = length args
              pushes = concatMap (\a -> genExpr a ++ [ins "push rax"]) args
              pops   = [ins ("pop " <> r) | r <- reverse (take n argRegs)]
           in pushes ++ pops ++
                [ ins "mov rbx, rsp"
                , ins "and rsp, -16"
                , ins "xor eax, eax"
                , ins ("call " <> l.text <> "@PLT")
                , ins "mov rsp, rbx" ]
    ESEQ s x -> genStmt s ++ genExpr x


genBinop :: BINOP -> [Text]
genBinop op =
  case op of
    PLUS  -> [ins "add rax, rcx"]
    MINUS -> [ins "sub rax, rcx"]
    MUL   -> [ins "imul rax, rcx"]
    DIV   -> [ins "cqo", ins "idiv rcx"]
    AND   -> [ins "and rax, rcx"]
    OR    -> [ins "or rax, rcx"]
    XOR   -> [ins "xor rax, rcx"]


jcc :: RELOP -> Text
jcc r = case r of
  REQ -> "je"
  RNE -> "jne"
  RLT -> "jl"
  RGT -> "jg"
  RLE -> "jle"
  RGE -> "jge"


genStmt :: STMT -> [Text]
genStmt s =
  case s of
    MOVE (MEM a) v ->
      genExpr v ++ [ins "push rax"] ++ genExpr a ++ [ins "pop rcx", ins "mov [rax], rcx"]
    MOVE _ _ -> error "unsupported MOVE destination"
    EXP x    -> genExpr x
    CJUMP op a b t f ->
      genExpr b ++ [ins "push rax"] ++ genExpr a ++
        [ ins "pop rcx"
        , ins "cmp rax, rcx"
        , ins (jcc op <> " " <> codeLbl t)
        , ins ("jmp " <> codeLbl f) ]
    JUMP l  -> [ins ("jmp " <> codeLbl l)]
    SEQ a b -> genStmt a ++ genStmt b
    LABEL l -> [codeLbl l <> ":"]


emitFragment :: Fragment -> CompilerM [Text]
emitFragment (Fragment l fr body ret) = do
  n <- readIORef fr.localsCount
  let frameBytes = ((word * n + 15) `div` 16) * 16
  pure $
    [ ins (".globl " <> l.text)
    , l.text <> ":"
    , ins "push rbp"
    , ins "mov rbp, rsp"
    , ins ("sub rsp, " <> tshow frameBytes)
    , ins "mov [rbp - 8], rbx" ]
    ++ genStmt body
    ++ maybe [] genExpr ret
    ++ [ ins "mov rbx, [rbp - 8]"
       , ins "mov rsp, rbp"
       , ins "pop rbp"
       , ins "ret" ]


-- | Whole program -> assembly text.
compileProgram :: Stmt -> CompilerM Text
compileProgram prog = do
  mainL <- toSymbol "a60_main"
  fr    <- newFrame mainL []
  let lvl0 = Level { parent = Nothing, frame = fr, depth = 0 }
      tenv = TEnv { vars = Map.empty, cur = lvl0, result = Nothing }
  body  <- transStmt tenv prog
  ref   <- asks (.fragmentsRef)
  frags <- readIORef ref
  texts <- mapM emitFragment (Fragment mainL fr body Nothing : reverse frags)
  pure $ Text.unlines $
    [".intel_syntax noprefix", ".text"]
    ++ concat texts
    ++ [".section .note.GNU-stack,\"\",@progbits"]


------------------------------------------------------------
-- # Monad
------------------------------------------------------------

type role CompilerM nominal
newtype CompilerM a = CompilerM (ReaderT Context IO a)
  deriving newtype
  ( Functor
  , Applicative
  , Monad
  , MonadReader Context
  , MonadIO
  , MonadUnliftIO)


runCompilerM :: CompilerM a -> ReaderT Context IO a
runCompilerM (CompilerM m) = m


------------------------------------------------------------
-- # Driver
------------------------------------------------------------

compile :: FilePath -> Text -> CompilerM Text
compile inp src = do
  parsed <- runParserT program inp src
  case parsed of
    Left err  -> liftIO (putStrLn (errorBundlePretty err) >> exitFailure)
    Right ast -> compileProgram ast


main :: IO ()
main = do
  args <- getArgs
  case args of
    [inp, out] -> do
      src <- Text.IO.readFile inp
      ctx <- newContext
      asm <- runReaderT (runCompilerM (compile inp src)) ctx
      Text.IO.writeFile out asm
    _ -> putStrLn "usage: algol60 input.a60 output.s" >> exitFailure
