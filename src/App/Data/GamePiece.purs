module Data.GamePiece
  ( GamePiece(..)
  , Star
  , Ship
  , Color(..)
  , Size(..)
  , PieceIndex(..)
  , all
  , getColor
  , getSize
  ) where

import Control.Apply (lift3)
import Data.List (List(..), (:))
import Prelude (class Eq, (&&), (==))

newtype GamePiece = GamePiece
  { color :: Color
  , size :: Size
  , index :: PieceIndex
  }

type Star = GamePiece
type Ship = GamePiece

getColor :: GamePiece -> Color
getColor (GamePiece { color }) = color

getSize :: GamePiece -> Size
getSize (GamePiece { size }) = size

instance gamePieceEq  :: Eq GamePiece where
  eq (GamePiece a) (GamePiece b) =
    a.color == b.color && a.size == b.size && a.index == b.index

gamepiece :: Color -> Size -> PieceIndex -> GamePiece
gamepiece color size index = GamePiece
  { color, size, index }

data Color
  = Red
  | Yellow
  | Green
  | Blue

derive instance colorEq :: Eq Color

data Size
  = Large
  | Medium
  | Small

derive instance sizeEq :: Eq Size

data PieceIndex
  = One
  | Two
  | Three

derive instance indexEq :: Eq PieceIndex

-- Create three copies of each combination of `Color` and `Size`
all :: List GamePiece
all =
  let
    colors = Yellow : Green : Blue : Red : Nil
    sizes = Small : Medium : Large : Nil
    indexes = One : Two : Three : Nil
  in
    lift3 gamepiece colors sizes indexes
