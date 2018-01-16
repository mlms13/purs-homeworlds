module Data.GamePiece
  ( GamePiece(..)
  , Star
  , Ship
  , Color(..)
  , Size(..)
  , all
  , getColor
  , getSize
  , gamepiece
  ) where

import Control.Apply (lift2)
import Data.List (List(..), (:))
import Prelude (class Eq, class Show, (&&), (<>), (==))

newtype GamePiece = GamePiece
  { color :: Color
  , size :: Size
  }

type Star = GamePiece
type Ship = GamePiece

getColor :: GamePiece -> Color
getColor (GamePiece { color }) = color

getSize :: GamePiece -> Size
getSize (GamePiece { size }) = size

instance gamePieceEq  :: Eq GamePiece where
  eq (GamePiece a) (GamePiece b) =
    a.color == b.color && a.size == b.size

gamepiece :: Color -> Size -> GamePiece
gamepiece color size = GamePiece
  { color, size }

data Color
  = Red
  | Yellow
  | Green
  | Blue

derive instance colorEq :: Eq Color
instance showColor :: Show Color where
  show Red = "red"
  show Yellow = "yellow"
  show Green = "green"
  show Blue = "blue"

data Size
  = Large
  | Medium
  | Small

derive instance sizeEq :: Eq Size
instance showSize :: Show Size where
  show Large = "large"
  show Medium = "medium"
  show Small = "small"


-- Create three copies of each combination of `Color` and `Size`
all :: List GamePiece
all =
  let
    colors = Yellow : Green : Blue : Red : Nil
    sizes = Small : Medium : Large : Nil
    oneCopy = lift2 gamepiece colors sizes
  in
    oneCopy <> oneCopy <> oneCopy
