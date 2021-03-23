{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import qualified Data.Text as T
import Position
import Symbol

data Operator
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

instance Show Operator where
  show = undefined

newtype ArgList = ArgList [Expr]

newtype FieldList = FieldList [(Symbol, Expr, Position)]

newtype Type = Type Symbol

newtype Test = Test Expr

newtype Body = Body Expr

newtype Then = Then Expr

newtype Else = Else Expr

newtype Low = Low Expr

newtype High = High Expr

newtype Size = Size Expr

newtype Init = Init Expr

data Expr where
  NilExpr :: Expr
  IntExpr :: Int -> Expr
  StringExpr :: T.Text -> Position -> Expr
  CallExpr :: Symbol -> ArgList -> Position -> Expr
  OpExpr :: Expr -> Operator -> Expr -> Position -> Expr
  RecordExpr :: FieldList -> Type -> Position -> Expr
  SeqExpr :: FieldList -> Expr
  AssignExpr :: Var -> Expr -> Position -> Expr
  IfExpr :: Test -> Then -> Else -> Position -> Expr
  WhileExpr :: Test -> Body -> Position -> Expr
  ForExpr :: Symbol -> Maybe Bool -> Low -> High -> Body -> Position -> Expr
  BreakExpr :: Symbol -> Expr
  LetExpr :: [Dec] -> Body -> Position -> Expr
  ArrayExpr :: Type -> Size -> Init -> Position -> Position -> Expr
  VarExpr :: Var -> Expr

data Var where
  SimpleVar :: Symbol -> Position -> Var
  FieldVar :: Var -> Symbol -> Position -> Var
  SubscriptVar :: Var -> Expr -> Position -> Var

data Dec

data Ty

data Field

data TypeDec

data FuncDec

instance Show Expr where
  show = undefined
