module Position
  ( Position (..)
  , dumpPosition
  )
where

import Data.Text qualified as Text

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
