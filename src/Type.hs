{-# LANGUAGE GADTs #-}

module Type where

import Data.IORef (IORef)

import Symbol (Symbol)


newtype Unique = IORef ()


data Type
  = Int
  | String
  | Record [(Symbol, Type)] Unique
  | Array Type Unique
  | Nil
  | Unit
  | Name Symbol Type (IORef (Maybe Type))
