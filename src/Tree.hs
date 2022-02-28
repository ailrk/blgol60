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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
module Tree where

-- Tree IR: a simple ir

data TODO a = TODOType


-- | IR expressions
data Expr
  = CONST Int
  | NAME (forall templabel .TODO templabel)
  | TEMP (forall temp. TODO temp)
  | BINOP Binop Expr Expr
  | MEM { addr :: Expr }

  | CALL
    { func :: Expr
    , args :: [Expr]
    }

  | ESEQ
    { sideEffect :: Stmt
    , expr       :: Expr
    }

-- | IR statement
data Stmt
  = MOVE { temp :: Expr, expr :: Expr }
  | EXP Expr
  | JUMP { addr :: Expr, labels :: (forall templabel .[TODO templabel])  }

  --  evaluate expr1 `ord` expr2, if true jumps to trueLabel, otherwise
  --  jumps to falseLabel.
  | CJUMP
    { ord       :: Relop
    , expr1     :: Expr
    , expr2     :: Expr
    , trueLabel :: (forall templabel . TODO templabel)
    , falseLabel :: (forall templabel . TODO templabel)
    }

  -- compose statements.
  | SEQ Stmt Stmt

  -- label is a statement too.
  | LABEL (forall templabel . TODO templabel)

data Binop = PLUS | MINUS | MUL | DIV | LSHIFT | RSHIFT | AND | OR | XOR
data Relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE

