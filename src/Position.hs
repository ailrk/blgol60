module Position
  ( Position (..)
  , getPosFromParsec
  , dumpPosition
  )
where

import Data.Text qualified as Text
import Text.Parsec (ParsecT)
import Text.Parsec.Pos qualified as P
import Text.Parsec.Prim qualified as P


data Position = Position
  { file :: Text
  , col :: Int
  , line :: Int
  }
  deriving (Eq, Show, Read)


dumpPosition :: Position -> Text
dumpPosition (Position file col line) =
  mconcat
    [ "<position: "
    , file
    , " line: "
    , Text.pack . show $ line
    , " col: "
    , Text.pack . show $ col
    , ">"
    ]


getPosFromParsec :: (Monad m) => ParsecT s u m Position
getPosFromParsec = do
  sourcePos <- P.getPosition
  pure $
    Position
      { file = Text.pack . P.sourceName $ sourcePos
      , col = P.sourceColumn sourcePos
      , line = P.sourceLine sourcePos
      }
