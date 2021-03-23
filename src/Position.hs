module Position
  ( Position(..),
  )
where

import qualified Data.Text as T

data Position = Position
  { file :: T.Text,
    startCol :: Int,
    startLine :: Int,
    endCol :: Int,
    endLine :: Int
  }

instance Show Position where
  show = undefined
