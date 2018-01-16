module Data.Bank
  ( Bank
  , has
  , hasColor
  , takePiece
  ) where

import Data.ActionValidation (ActionError(..))
import Data.Either (Either(..))
import Data.Foldable (any, elem, foldl)
import Data.GamePiece (Color, GamePiece, Size, getColor, getSize)
import Data.List (List(Nil), (:))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (map, ($), (&&), (<<<), (==))

type Bank = List GamePiece

-- The bank contains any piece of the given size and color
has :: Size -> Color -> Bank -> Boolean
has s c =
  any (\gp -> (getSize gp) == s && (getColor gp) == c)

-- The bank has any piece of the given color
hasColor :: Color -> Bank -> Boolean
hasColor c =
  any ((==) c) <<< map getColor

-- The bank has a specific piece
hasPiece :: GamePiece -> Bank -> Boolean
hasPiece = elem

-- Try to remove a specific piece from the bank, returning it and
-- the updated bank, or an error if the piece isn't in the bank
takePiece :: GamePiece -> Bank -> Either ActionError (Tuple GamePiece Bank)
takePiece gp bank =
  let
    withoutOne = foldl f (Tuple Nil true) bank
    pred = (==) gp
    f (Tuple list shouldFilter) piece =
      if pred piece && shouldFilter then
        Tuple list false
      else
        Tuple (piece : list) shouldFilter
  in
  case snd withoutOne of
    false -> Right $ Tuple gp $ fst withoutOne
    true -> Left $ PieceNotInBank gp

