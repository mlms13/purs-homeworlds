module Data.Game
  ( Game(..)
  , SetupState(..)
  , APrimary(..)
  , BPrimary(..)
  , ASecondary(..)
  , BSecondary(..)
  , PlayingState
  , ActionType(..)
  , playingState
  ) where

import Data.Bank (Bank)
import Data.GamePiece (Color, Size, Star)
import Data.Player (Player)
import Data.StarSystem (BinarySystem)
import Data.Tuple (Tuple)
import Data.Universe (Universe)

data Game
  = Setup Bank SetupState
  | Playing Bank PlayingState
  | Finished { winner :: Player }

newtype APrimary = APrimary Star
newtype BPrimary = BPrimary Star
newtype ASecondary = ASecondary Star
newtype BSecondary = BSecondary Star

data SetupState
  = AChoosingPrimary
  | BChoosingPrimary APrimary
  | AChoosingSecondary APrimary BPrimary
  | BChoosingSecondary APrimary BPrimary ASecondary
  | AChoosingShip APrimary BPrimary ASecondary BSecondary
  | BChoosingShip BinarySystem BPrimary BSecondary

type PlayingState =
  { turn :: Tuple Player ActionType
  , universe :: Universe
  }

playingState :: (Tuple Player ActionType) -> Universe -> PlayingState
playingState turn universe =
  { turn : turn
  , universe : universe
  }

data ActionType
  = AnySingle
  | Constrained Size Color
