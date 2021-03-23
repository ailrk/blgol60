{-# LANGUAGE RankNTypes #-}

module Tree where

import Temp qualified


-- Tree IR: a simple ir

type Label = Temp.Label
type Size = Int


data TODO a = TODOType


data Stmt
  = MOVE Expr Expr
  | EXP Expr
  | CJUMP Stmt_CJUMP
  | JUMP [(Expr, Label)]
  | SEQ Stmt Stmt
  | LABEL Label


data Stmt_CJUMP = StmtCJUMP
  { rlop :: Relop
  , lhs :: Expr
  , rhs :: Expr
  , trueLabel :: Label
  , falseLabel :: Label
  }


data Expr
  = CONST Int
  | NAME Label
  | TEMP Temp.Temp
  | BINOP Binop Expr Expr
  | MEM Expr
  | CALL [(Expr, Expr)]
  | ESEQ Stmt Expr


data Binop = PLUS | MINUS | MUL | DIV | LSHIFT | RSHIFT | AND | OR | XOR
data Relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE
