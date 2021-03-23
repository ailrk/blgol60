module Tree
  ( Label
  , Size
  , STMT(..)
  , STMTCJUMP(..)
  , EXPR(..)
  , BINOP(..)
  , RELOP(..)
  , TODO(..)
  )
where

import Temp qualified


-- Tree IR: a simple ir

type Label = Temp.Label
type Size = Int


data TODO a = TODOType


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
  | TEMP Temp.Temp
  | BINOP BINOP EXPR EXPR
  | MEM EXPR
  | CALL [(EXPR, EXPR)]
  | ESEQ STMT EXPR


data BINOP = PLUS | MINUS | MUL | DIV | LSHIFT | RSHIFT | AND | OR | XOR
data RELOP = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE
