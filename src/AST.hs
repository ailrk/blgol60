{-# LANGUAGE DataKinds             #-}
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
--
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}

module AST where

import           Data.Kind
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Text          as T
import qualified GHC.TypeLits       as TL
import           Position
import           Symbol

-------------------------------------------------------------------------------
-- Variable
-------------------------------------------------------------------------------

data Var

  -- identifier
  = SimpleVar
    { name     :: Symbol
    , position :: Position
    }

  -- a[integer]
  | SubscriptVar
    { subVar    :: Var
    , subscript :: [Expr]
    , position  :: Position
    }

  deriving Show

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

-- Algo60 only have Int, real, and string three primitive types.
data Expr
  = NilExpr

  | VarExpr Var Position

  -- scalaras
  | IntExpr Integer Position
  | RealExpr Double Position
  | StringExpr T.Text Position

  -- different from val, label is just a symbol.
  | LabelExpr Symbol Position
  | SwitchExpr Symbol Expr Position

  | CallExpr
    { callName :: Symbol
    , callArgs :: [Expr]
    , position :: Position
    }

  -- note there is both if expr and if statement
  | IfExpr
    { test     :: Expr
    , thenExpr :: Expr
    , elseExpr :: Expr
    , position :: Position
    }

  | BinopExpr BinaryOp Position Expr Expr

  | UnopExpr UnaryOp Position Expr

  deriving Show

-------------------------------------------------------------------------------
-- Statements
-------------------------------------------------------------------------------
data Stmt
  = NilStmt

  | BlockStmt
      { decs     :: [Dec]
      , body     :: Stmt
      , position :: Position
      }

  | ArrayStmt
      { typ           :: Symbol
      , size          :: Expr
      , init          :: Expr
      , positionBegin :: Position
      , positionEnd   :: Position
      }

  | VarStmt Var

  -- imperative basics
  | SeqStmt { fields :: [(Symbol, Expr, Position)] }

  | AssignStmt Var Expr Position

  -- control flow
  | IfStmt
    { test     :: Expr
    , thenStmt :: Stmt
    , elseStmt :: Stmt
    , position :: Position
    }

  | ForStmt
    { name      :: Symbol
    , forHeader :: ForStmtHeader
    , flag      :: Bool
    , body      :: Stmt
    , position  :: Position
    }

  -- goto label
  | GoToStmt Symbol

  deriving Show


-- for has three versions: use it as block;  step-until; while
data ForStmtHeader
  = While
    { initAssigns :: [Expr]
    , test :: Expr
    }
  | Step
    { initAssigns :: [Expr]
    , step :: Expr
    , until :: Expr
    }
  | Immediate
  deriving Show

-------------------------------------------------------------------------------
-- Declarations
-------------------------------------------------------------------------------
data Dec
  = ProcedureDec [FuncDec]

  | VarDec
    { name :: Symbol
    , escape :: Bool
    , typs :: Maybe (Symbol, Position)
    , init :: Expr
    , position :: Position
    }

  | TypeDec
    { name :: Symbol
    , typ :: Ty
    , position :: Position
    }
  deriving Show

-- type annotations
data Ty
  -- e.g integer a;
  = NameTy Symbol Position

  -- e.g integer array a[10];
  | ArrayTy Symbol Position
  deriving Show

-- OPerators
data BinaryOp = PlustOp | MinusOp | TimesOp | DivideOp
              | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
              | AndOp | OrOp | XorOp
  deriving (Eq, Show)

data UnaryOp = NotOp deriving (Eq, Show)

-- helps
data Field
  = Field
    { name :: Symbol
    , escape :: Bool
    , typ :: Symbol
    , position :: Position }
  deriving Show

data FuncDec
  = FuncDec
    { name :: Symbol
    , parameters :: [(Field, EvalStrat)]
    , result :: Maybe (Symbol, Position)
    , body :: Expr
    , position :: Position
    }
  deriving Show

data EvalStrat = CBV | CBN deriving Show

-------------------------------------------------------------------------------
-- enums
-------------------------------------------------------------------------------
toBinaryOp :: T.Text -> BinaryOp
toBinaryOp "+"   = PlustOp
toBinaryOp "-"   = MinusOp
toBinaryOp "*"   = TimesOp
toBinaryOp "/"   = DivideOp
toBinaryOp "="   = EqOp
toBinaryOp "/="  = NeqOp
toBinaryOp "<"   = LtOp
toBinaryOp "<="  = LeOp
toBinaryOp ">"   = GtOp
toBinaryOp ">="  = GeOp
toBinaryOp "and" = AndOp
toBinaryOp "or"  = OrOp
toBinaryOp "xor" = XorOp

toUnaryOp :: T.Text -> UnaryOp
toUnaryOp "not" = NotOp
