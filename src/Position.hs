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

{-# LANGUAGE TemplateHaskell #-}
module Position
  ( Position(..)
  , getPosFromParsec
  )
where

import qualified Data.Text        as T
import qualified Text.Parsec.Pos  as P
import qualified Text.Parsec.Prim as P
import Debug.Trace (trace)

data Position = Position
  { file :: T.Text
  , col  :: Int
  , line :: Int
  }

instance Show Position where
  show (Position f c l) = "<loc: " ++ show f ++ " line: " ++ show l
                       ++ " col: " ++ show c ++ ">"

-- reuse parsec positino directly.
getPosFromParsec :: Monad m => P.ParsecT s u m Position
getPosFromParsec = do
  pos <- P.getPosition
  let name = P.sourceName pos
  let col = P.sourceColumn pos
  let line = P.sourceLine pos
  return $ Position (T.pack name) col line
