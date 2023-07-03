{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.Text (Text)
import Position (Position)
import Symbol (Symbol)


-------------------------------------------------------------------------------
-- Variable
-------------------------------------------------------------------------------

data Var
  = SimpleVar
      Symbol
      -- ^ name
      Position
      -- ^ posiiton
  | SubscriptVar
      Var
      -- ^ subVar
      [Expr]
      -- ^ subscript
      Position
      -- ^ position
  deriving (Show, Read, Eq)


-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

-- Algo60 only have Int, real, and string three primitive types.
data Expr
  = NilExpr
  | VarExpr Var Position
  | -- scalaras
    IntExpr Integer Position
  | RealExpr Double Position
  | StringExpr Text Position
  | -- different from val, label is just a symbol.
    LabelExpr Symbol Position
  | SwitchExpr Symbol Expr Position
  | CallExpr
      Symbol
      -- ^ callName
      [Expr]
      -- ^ callArgs
      Position
      -- ^ position
      -- note there is both if expr and if statement
  | IfExpr
      Expr
      -- ^ test
      Expr
      -- ^ thenExpr
      Expr
      -- ^ elseExpr
      Position
      -- ^ Position
  | BinopExpr BinaryOp Position Expr Expr
  | UnopExpr UnaryOp Position Expr
  deriving (Show, Read, Eq)


-------------------------------------------------------------------------------
-- Statements
-------------------------------------------------------------------------------
data Stmt
  = NilStmt
  | BlockStmt BlockStmt
  | ArrayStmt ArrayStmt
  | CallStmt CallStmt
  | LabelStmt Symbol Stmt Position
  | VarStmt Var
  | -- | fields
    SeqStmt Stmt Stmt
  | AssignStmt [Var] Expr Position
  | IfStmt IfStmt
  | ForStmt ForStmt
  | GoToStmt Symbol
  deriving (Show, Read, Eq)


data BlockStmt = BlockStmt_
  { decs :: [Dec]
  , body :: Stmt
  , position :: Position
  }
  deriving (Show, Read, Eq)


data ArrayStmt = ArrayStmt_
  { typ :: Symbol
  , size :: Expr
  , init :: Expr
  , positionBegin :: Position
  , positionEnd :: Position
  }
  deriving (Show, Read, Eq)


data CallStmt = CallStmt_
  { name :: Symbol
  , parameters :: [Expr]
  , position :: Position
  }
  deriving (Show, Read, Eq)


data IfStmt = IfStmt_
  { test :: Expr
  , thenBranch :: Stmt
  , elseBranch :: Stmt
  , position :: Position
  }
  deriving (Show, Read, Eq)


data ForStmt = ForStmt_
  { name :: Var
  , forListElements :: [ForListElement]
  -- ^ for list element. It's in reversed order as it's written in the source code.
  --
  --  e.g if the statement is `for 1, 2, step 1 until 4, 1, 2`
  --  the list will be [2, 1, 2 step 1 until 4, 1]
  , loopBody :: Stmt
  -- ^ loop body
  , position :: Position
  -- ^ position
  }
  deriving (Show, Read, Eq)


data ForListElement
  = While
      Expr
      -- ^ arithmetic expr
      Expr
      -- ^ test
  | Step
      Expr
      -- ^ init arithmetic expr
      Expr
      -- ^ step
      Expr
      -- ^ until
  | Immediate Expr
  deriving (Show, Read, Eq)


-------------------------------------------------------------------------------
-- Declarations
-------------------------------------------------------------------------------
data Dec
  = ProcedureDec Procedure
  | TypeDec
      Bool
      -- ^ local or own
      Type
      -- ^ type declaration lists
      [Symbol]
      -- ^ variable name list
      Position
      -- ^ position
  | ArrayDec
      Bool
      -- ^ local or own
      (Maybe Type)
      -- ^ type
      [ArraySegment]
      -- ^ variable name list
      Position
      -- ^  position
  | SwitchDec
      Symbol
      -- ^ switch identifier
      [Expr]
      -- ^ designational expression
      Position
      -- ^ position
  deriving (Show, Read, Eq)


data ArraySegment = ArraySegment
  { name :: [Symbol]
  , boundPairs :: [(Expr, Expr)]
  }
  deriving (Show, Read, Eq)


-- type annotations
data Type
  = ScalarT Symbol
  | ArrayT
  | SwitchT
  | StringT
  | ProcT (Maybe Type)
  | LabelT
  deriving (Show, Read, Eq)


-- OPerators
data BinaryOp
  = PlustOp
  | MinusOp
  | TimesOp
  | DivideOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  | AndOp
  | OrOp
  | XorOp
  deriving (Eq, Show, Read)


data UnaryOp = NotOp deriving (Eq, Show, Read)


data Procedure = Procedure
  { name :: Symbol
  , parameters :: [Parameter]
  , returnType :: Maybe Type
  , body :: Stmt
  , position :: Position
  }
  deriving (Show, Read, Eq)


data Parameter = Paramter
  { name :: Symbol
  , typ :: Maybe Type
  , evalStrat :: EvalStrat
  }
  deriving (Show, Read, Eq)


data EvalStrat = CBV | CBN deriving (Show, Read, Eq)


-------------------------------------------------------------------------------
-- enums
-------------------------------------------------------------------------------
toBinaryOp :: Text -> Maybe BinaryOp
toBinaryOp "+" = Just PlustOp
toBinaryOp "-" = Just MinusOp
toBinaryOp "*" = Just TimesOp
toBinaryOp "/" = Just DivideOp
toBinaryOp "=" = Just EqOp
toBinaryOp "/=" = Just NeqOp
toBinaryOp "<" = Just LtOp
toBinaryOp "<=" = Just LeOp
toBinaryOp ">" = Just GtOp
toBinaryOp ">=" = Just GeOp
toBinaryOp "and" = Just AndOp
toBinaryOp "or" = Just OrOp
toBinaryOp "xor" = Just XorOp
toBinaryOp _ = Nothing


toUnaryOp :: Text -> Maybe UnaryOp
toUnaryOp "not" = Just NotOp
toUnaryOp _ = Nothing
