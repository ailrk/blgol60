{-# LANGUAGE DeriveAnyClass #-}
module Position
  ( Position (..)
  , getPosition
  )
where

import Text.Megaparsec (Pos, TraversableStream, ParsecT, SourcePos (..), getSourcePos)

data Position = Position
  { file :: FilePath
  , col :: !Pos
  , line :: !Pos
  }
  deriving (Eq, Show, Read, Generic, NFData)


getPosition :: (TraversableStream s, Ord e) => ParsecT e s m Position
getPosition = do
  SourcePos {..} <- getSourcePos
  pure Position
    { file = sourceName
    , line = sourceLine
    , col = sourceColumn
    }

