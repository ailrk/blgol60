module Type
  ( Type(..)
  , Unique(..)
  )
where

import Prelude hiding (Type)
import Symbol (Symbol)

newtype Unique = Unique { unUnique :: IORef () }


data Type
  = Int
  | String
  | Record [(Symbol, Type)] Unique
  | Array Type Unique
  | Nil
  | Unit
  | Name Symbol Type (IORef (Maybe Type))
