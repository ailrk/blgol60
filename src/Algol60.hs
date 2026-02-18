{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Algol60 where

import Data.Hashable qualified as Hashable
import Data.Map qualified as Map
import Text.Megaparsec (Pos, TraversableStream, ParsecT, SourcePos (..), getSourcePos, (<|>), many, some, optional)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (Map)
import UnliftIO (MonadIO, IORef, readIORef, writeIORef, newIORef, MonadUnliftIO, modifyIORef)
import Control.Monad.Reader (MonadReader, asks, ReaderT, MonadTrans (..))
import Control.DeepSeq (NFData)
import Control.Monad.Combinators (skipMany)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Generics.Labels ()
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Megaparsec (MonadParsec (try), sepEndBy1, lookAhead, sepBy1, between, manyTill, sepBy, choice, anySingle, satisfy, (<?>))
import Text.Megaparsec.Char ( letterChar, alphaNumChar, space1, char, space, newline, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Void (Void)
import Control.Lens ((<&>))
import Data.Maybe (isJust, maybeToList, fromMaybe)
import Control.Monad (join, void)
import Data.Bifunctor (Bifunctor(..))
import Data.List (find)


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
      { unop       :: UnaryOp
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
      -- ^ for list element. It's in reversed order as it's written in the source code.
      --
      --  e.g if the statement is `for 1, 2, step 1 until 4, 1, 2`
      --  the list will be [2, 1, 2 step 1 until 4, 1]
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
      , position :: Position
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
  [ "begin", "end", "own", "real", "integer", "bool", "array", "switch", "procedure"
  , "value", "string", "label", "goto", "if", "then", "else", "do", "for", "or", "and"
  , "not", "equiv", "impl", "step", "while", "comment", "true", "false"
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


-- keywords
reserved :: Text -> Parser ()
reserved w = symbol w *> pure ()


integer :: Parser Expr
integer = IntExpr <$> lexeme Lexer.decimal <*> getPosition


float :: Parser Expr
float = RealExpr <$> lexeme Lexer.float <*> getPosition


-- string literals
stringLiteral :: Parser Expr
stringLiteral =
  StringExpr
  <$> Text.pack <$> (char '"' >> manyTill Lexer.charLiteral (char '"'))
  <*> getPosition


whiteSpace :: Parser ()
whiteSpace = Lexer.space space1 mempty (Lexer.skipBlockComment "comment" ";")


-- look ahead without consuming. Useful for debugging
currentChar :: Parser Char
currentChar = lookAhead anySingle


commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")


commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p (symbol ",")


symbol :: Text -> Parser Symbol
symbol s = Lexer.symbol whiteSpace s >>= lift . toSymbol


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")


logicValue :: Parser Expr
logicValue = BoolExpr <$> c <*> getPosition
  where
    c = (symbol "true" *> pure True) <|> symbol "false" *> pure False


label :: Parser Symbol
label = identifier


switchIdentifier :: Parser Symbol
switchIdentifier = identifier


arrayIdentifier :: Parser Symbol
arrayIdentifier = identifier


procedureIdentifier :: Parser Symbol
procedureIdentifier = identifier


ifClause :: Parser Expr
ifClause = do
  symbol "if" *> booleanExpression <* symbol "then"


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
  try simpleArithmeticExpression
  <|> try (ifClause
          *> simpleArithmeticExpression
          *> symbol "else"
          *> arithmeticExpression)


binary :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text CompilerM) a
binary  name f = InfixL (f <$ symbol name)


binaryR :: Text -> (a -> a -> a) -> Operator (ParsecT Void Text CompilerM) a
binaryR  name f = InfixR (f <$ symbol name)


prefix :: Text -> (a -> a) -> Operator (ParsecT Void Text CompilerM) a
prefix  name f = Prefix (f <$ symbol name)


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
booleanExpression = do
  try simpleBooleanExpression
  <|> try (ifClause
        *> simpleBooleanExpression
        *> symbol "else"
        *> booleanExpression)


simpleBooleanExpression :: Parser Expr
simpleBooleanExpression = do
  pos <- getPosition
  makeExprParser term
    [ [ prefix "~" (UnopExpr NotOp pos) ]
    , [ prefix "not" (UnopExpr NotOp pos) ]
     , [ binary "or" (BinopExpr OrOp pos) ]
     , [ binary "and" (BinopExpr AndOp pos) ]
     , [ binary "impl" (BinopExpr ImplOp pos) ]
     , [ binary "equiv" (BinopExpr EquivOp pos) ]
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
designationalExpression = do
  try simpleDesignationalExpression
  <|> try (ifClause
        *> simpleDesignationalExpression
        *> symbol "else"
        *> designationalExpression)


simpleDesignationalExpression :: Parser Expr
simpleDesignationalExpression =
  try (SwitchExpr <$> switchIdentifier *> brackets subscriptExpression)
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
  try (symbol "real" *> pure RealT)
  <|> try (symbol "integer" *> pure IntegerT)
  <|> symbol "boolean" *> pure BooleanT


localOrOwn :: Parser Bool
localOrOwn = optional (symbol "own") <&> isJust


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
  try (lookAhead (reserved "end")) <|> try (lookAhead (symbol ";" *> pure ()))
  pure DummyStmt


forListElement :: Parser ForListElement
forListElement = do
  try (Step <$> arithmeticExpression <* reserved "step" <*> arithmeticExpression <* reserved "until" <*> arithmeticExpression)
  <|> try (While <$> arithmeticExpression <* reserved "while" <*> booleanExpression)
  <|> (Immediate <$> arithmeticExpression)


forList :: Parser [ForListElement]
forList = commaSep1 forListElement


forStatement :: Parser Stmt
forStatement = do
  try (label *> symbol ":" *> forStatement) <|> (forClause <*> statement <*> getPosition)
  where
    forClause = do
      reserved "for"
      name <- variable
      _ <- symbol ":="
      forListElements  <- forList
      reserved "do"
      pure (ForStmt name forListElements)


conditionalStatement :: Parser Stmt
conditionalStatement = do
  try (label *> symbol ":" *> conditionalStatement)
  <|> conditionalStatement'
  where
    conditionalStatement' = ifClause *>
      (try forStatement
       <|> try (unconditionalStatement *> reserved "else" *> statement)
       <|> unconditionalStatement)


compoundTail :: Parser Stmt
compoundTail = SeqStmt <$> statement <*> next <?> "compoundTail"
  where
    endComment :: Parser ()
    endComment =
      let notComma = satisfy (/= ';')
          end = void (lookAhead (char ';'))
                  <|> void (lookAhead (string "end"))
       in void . optional $ manyTill notComma end

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
block = try (label *> symbol ":" *> block) <|> unlabeledBlock <?> "block"


program :: Parser Stmt
program = do
  emptyLines
  stmt <- try block <|> compoundStatement
  emptyLines
  pure stmt
  where
    emptyLines = skipMany (void space1 <|> void (newline *> space))


------------------------------------------------------------
-- # Tree
------------------------------------------------------------

type Size  = Int


data STMT
  = MOVE EXPR EXPR
  | EXP EXPR
  | CJUMP STMTCJUMP
  | JUMP [(EXPR, Label)]
  | SEQ STMT STMT
  | LABEL Label


data STMTCJUMP = StmtCJUMP
  { rlop       :: RELOP
  , lhs        :: EXPR
  , rhs        :: EXPR
  , trueLabel  :: Label
  , falseLabel :: Label
  }


data EXPR
  = CONST Int
  | NAME Label
  | TEMP Temp
  | BINOP BINOP EXPR EXPR
  | MEM EXPR
  | CALL [(EXPR, EXPR)]
  | ESEQ STMT EXPR


data BINOP = PLUS | MINUS | MUL | DIV | LSHIFT | RSHIFT | AND | OR | XOR
data RELOP = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE


------------------------------------------------------------
-- # Temp
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
-- # Temp
------------------------------------------------------------


-- | Represents a virtual memory location
type Label = Symbol

-- | Represent a virtual registor that holds a temporary value.
data Temp = Temp Int (Maybe Text.Text) deriving (Show, Eq)


-- unique memory address label
newNamedLabel :: (MonadReader Context m, MonadIO m) => Text -> m Label
newNamedLabel name = do
  ref <- asks (.labelCounter)
  modifyIORef ref (+ 1)
  i <- readIORef ref
  toSymbol (Text.concat [name, Text.pack (show i)])


newLabel :: (MonadReader Context m, MonadIO m) => m Label
newLabel = newNamedLabel "l"


-- unique memory address label
newNamedTemp :: (MonadReader Context m, MonadIO m) => Text -> m Label
newNamedTemp name = do
  ref <- asks (.tempCounter)
  modifyIORef ref (+ 1)
  i <- readIORef ref
  toSymbol (Text.concat [name, Text.pack (show i)])


newTemp :: (MonadReader Context m, MonadIO m) => m Label
newTemp = newNamedTemp "t"

------------------------------------------------------------
-- # Context
------------------------------------------------------------


data Context = Context
  { symbolTableRef :: IORef SymbolTable
  , tempCounter    :: IORef Int
  , labelCounter   :: IORef Int
  }


newContext :: MonadIO m => m Context
newContext = do
  symbolTableRef <- newIORef newSymbolTable
  tempCounter    <- newIORef 0
  labelCounter   <- newIORef 0
  pure Context {..}


------------------------------------------------------------
-- # Semantic
------------------------------------------------------------


data Access
  = InReg Temp -- variable can live in a register
  | InFrame Int -- variable must live on stack as the offset from FP
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
      , level  :: Level
      , strat  :: EvalStrat
      }
  | ProcEntry
      { label  :: Label
      , level  :: Level
      , params :: [Param]
      , retTy :: Maybe Type
      }


data Frame = Frame
  { name        :: Label      -- asm label for procedure
  , formals     :: [Access]   -- how to locate each parameter
  , localsCount :: IORef Int  -- offset for new local var
  }


type Env = Map Symbol EnvEntry


data Target = Target
  { wordSize :: Int
  }


targetX86_64 :: Target
targetX86_64 = Target
  { wordSize = 8
  }


-- | Create a new level, the first formal is aways the static link
newLevel :: Target -> Level -> Label -> [Bool] -> CompilerM Level
newLevel target parent name escapes = do
  let actualEscapes = True : escapes
  fr <- newFrame target name actualEscapes
  pure Level
    { parent = Just parent
    , frame  = fr
    , depth  = parent.depth + 1
    }


-- | Create a Frame
newFrame :: Target -> Label -> [Bool] -> CompilerM Frame
newFrame target name escapes = do
  ref <- newIORef 0
  let formals = zipWith (\esc i ->
                  if esc
                    then InFrame (i * target.wordSize)
                    else InReg (Temp i (Just "p")))
                  escapes [0..]
  pure Frame
    { name        = name
    , formals     = formals
    , localsCount = ref
    }


-- | Follow static links from the current level to the level where the
-- variable is defined.
followLinks :: Level -> Level -> EXPR -> EXPR
followLinks current dest base
  | current.depth == dest.depth = base
  | otherwise = case current.parent of
                  Just p -> followLinks p dest (MEM base)
                  Nothing -> error "Scope error: Desination level not an ancestor"


transExpr :: Env -> Expr -> CompilerM EXPR
transExpr env expr =
  case expr of
    VarExpr var ->
      case var of
        SimpleVar name pos ->
          case Map.lookup name env of
            Just (e@VarEntry{}) -> do
              let fp = TEMP (Temp 0 (Just "fp"))
              let addr = undefined -- followLinks e.curre
              pure case e.access of
                     InReg _ -> undefined
                     InFrame _ -> undefined
            Just (ProcEntry { }) -> do
              undefined
            Nothing -> error ("Undefined variable at " ++ show pos)
        SubscriptVar {} -> undefined
        FunctionDesignator {} -> undefined
    BinopExpr op pos l r -> do
      undefined
    IntExpr n pos -> undefined
    RealExpr n pos -> undefined
    BoolExpr n pos -> undefined
    StringExpr n pos -> undefined
    LabelExpr { } -> undefined
    SwitchExpr { } -> undefined
    CallExpr { } -> undefined
    IfExpr { } -> undefined
    UnopExpr {} -> undefined
    NilExpr -> undefined


transStmt :: Env -> Stmt -> CompilerM STMT
transStmt env stmt =
  case stmt of
    BlockStmt {} -> undefined
    CallStmt {} -> undefined
    LabelStmt {} -> undefined
    VarStmt var -> undefined
    SeqStmt s1 s2 -> undefined
    AssignStmt lhs rhs pos -> undefined
    IfStmt {} -> undefined
    ForStmt {} -> undefined
    GotoStmt {} -> undefined
    DummyStmt -> undefined


------------------------------------------------------------
-- # Monad
------------------------------------------------------------


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
