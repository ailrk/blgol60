module AST
  ( Var(..)
  , SimpleVar(..)
  , SubscriptVar(..)
  , Expr(..)
  , Stmt(..)
  , BlockStmt(..)
  , ArrayStmt(..)
  , CallStmt(..)
  , LabelStmt(..)
  , AssignStmt(..)
  , IfStmt(..)
  , ForStmt(..)
  , ForListElement(..)
  , Dec(..)
  , TypeDec(..)
  , ArrayDec(..)
  , ArraySegment(..)
  , SwitchDec(..)
  , Type(..)
  , BinaryOp(..)
  , UnaryOp(..)
  , ProcedureDec(..)
  , Parameter(..)
  , EvalStrat(..)
  , toBinaryOp
  , toUnaryOp
  )
where

import Prelude hiding (Type)
import Position (Position)
import Symbol (Symbol)


----------------------------------------
-- Variable
----------------------------------------

data Var
  = SimpleVar SimpleVar
  | SubscriptVar SubscriptVar
  deriving (Show, Read, Eq)


data SimpleVar = SimpleVar_
  { name     :: Symbol
  , position :: Position
  }
  deriving (Show, Read, Eq, Generic)


data SubscriptVar = SubscriptVar_
  { name       :: Var
  , subscripts :: [Expr]
  , position   :: Position
  }
  deriving (Show, Read, Eq, Generic)


----------------------------------------
-- Expressions
----------------------------------------

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
      -- | callName
      Symbol
      -- | callArgs
      [Expr]
      -- | position
      -- note there is both if expr and if statement
      Position
  | IfExpr
      -- | test
      Expr
      -- | thenExpr
      Expr
      -- | elseExpr
      Expr
      -- | Position
      Position
  | BinopExpr BinaryOp Position Expr Expr
  | UnopExpr UnaryOp Position Expr
  deriving (Show, Read, Eq)


----------------------------------------
-- Statements
----------------------------------------
data Stmt
  = NilStmt
  | BlockStmt BlockStmt
  | ArrayStmt ArrayStmt
  | CallStmt CallStmt
  | LabelStmt LabelStmt
  | VarStmt Var
  | SeqStmt Stmt Stmt
  | AssignStmt AssignStmt
  | IfStmt IfStmt
  | ForStmt ForStmt
  | GoToStmt Symbol
  deriving (Show, Read, Eq)


data BlockStmt = BlockStmt_
  { decs :: [Dec]
  , body :: Stmt
  , position :: Position
  }
  deriving (Show, Read, Eq, Generic)


data ArrayStmt = ArrayStmt_
  { typ           :: Symbol
  , size          :: Expr
  , initValue     :: Expr
  , positionBegin :: Position
  , positionEnd   :: Position
  }
  deriving (Show, Read, Eq, Generic)


data CallStmt = CallStmt_
  { name       :: Symbol
  , parameters :: [Expr]
  , position   :: Position
  }
  deriving (Show, Read, Eq, Generic)


data LabelStmt = LabelStmt_
  { name      :: Symbol
  , statement :: Stmt
  , position  :: Position
  }
  deriving (Show, Read, Eq, Generic)


data AssignStmt = AssignStmt_
  { lhs      :: [Var]
  , rhs      :: Expr
  , position :: Position
  }
  deriving (Show, Read, Eq, Generic)


data IfStmt = IfStmt_
  { test       :: Expr
  , thenBranch :: Stmt
  , elseBranch :: Stmt
  , position   :: Position
  }
  deriving (Show, Read, Eq, Generic)


data ForStmt = ForStmt_
  { name            :: Var
  , forListElements :: [ForListElement]
  -- ^ for list element. It's in reversed order as it's written in the source code.
  --
  --  e.g if the statement is `for 1, 2, step 1 until 4, 1, 2`
  --  the list will be [2, 1, 2 step 1 until 4, 1]
  , loopBody        :: Stmt
  -- ^ loop body
  , position        :: Position
  -- ^ position
  }
  deriving (Show, Read, Eq, Generic)


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


----------------------------------------
-- Declarations
----------------------------------------
data Dec
  = ProcedureDec ProcedureDec
  | TypeDec TypeDec
  | ArrayDec ArrayDec
  | SwitchDec SwitchDec
  deriving (Show, Read, Eq)


data TypeDec = TypeDec_
  { own         :: Bool
  , typeDecList :: [Type]
  , nameList    :: [Symbol]
  , position    :: Position
  }
  deriving (Show, Read, Eq)


data ArrayDec = ArrayDec_
  { own       :: Bool
  , arrayType :: Maybe Type
  , segments  :: [ArraySegment]
  , position  :: Position
  }
  deriving (Show, Read, Eq, Generic)


data ArraySegment = ArraySegment
  { names :: [Symbol]
  , bound :: (Expr, Expr)
  }
  deriving (Show, Read, Eq, Generic)


data SwitchDec = SwitchDec_
  { name               :: Symbol
  , designationalExprs :: [Expr]
  , position           :: Position
  }
  deriving (Show, Read, Eq, Generic)


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
  = PlusOp
  | MinusOp
  | TimesOp
  | DivOp
  | IntDivOp
  | ExpOp
  | EqOp
  | NeqOp
  | LtOp
  | LeOp
  | GtOp
  | GeOp
  | AndOp
  | OrOp
  | EquivOp
  | ImplOp
  deriving (Eq, Show, Read)


data UnaryOp
  = NotOp
  | BitNotOp
    deriving (Eq, Show, Read)


data ProcedureDec = ProcedureDec_
  { name :: Symbol
  , parameters :: [Parameter]
  , returnType :: Maybe Type
  , body :: Stmt
  , position :: Position
  }
  deriving (Show, Read, Eq, Generic)


data Parameter = Parameter
  { name      :: Symbol
  , typ       :: Maybe Type
  , evalStrat :: EvalStrat
  }
  deriving (Show, Read, Eq, Generic)


data EvalStrat = CBV | CBN deriving (Show, Read, Eq)


----------------------------------------
-- enums
----------------------------------------

toBinaryOp :: Text -> Maybe BinaryOp
toBinaryOp "+" = Just PlusOp
toBinaryOp "-" = Just MinusOp
toBinaryOp "*" = Just TimesOp
toBinaryOp "/" = Just DivOp
toBinaryOp "//" = Just IntDivOp
toBinaryOp "**" = Just ExpOp
toBinaryOp "=" = Just EqOp
toBinaryOp "~="  = Just NeqOp
toBinaryOp "<" = Just LtOp
toBinaryOp "<=" = Just LeOp
toBinaryOp ">" = Just GtOp
toBinaryOp ">=" = Just GeOp
toBinaryOp "and" = Just AndOp
toBinaryOp "or" = Just OrOp
toBinaryOp "equiv" = Just EquivOp
toBinaryOp "impl" = Just ImplOp
toBinaryOp _ = Nothing


toUnaryOp :: Text -> Maybe UnaryOp
toUnaryOp "not" = Just NotOp
toUnaryOp "~" = Just BitNotOp
toUnaryOp _ = Nothing
