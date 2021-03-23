module Symbol
  ( Symbol (..),
  )
where

import qualified Data.Text as T

data Symbol = Symbol
  { name :: T.Text,
    symbol :: Int
  }

instance Show Symbol where
  show = undefined
