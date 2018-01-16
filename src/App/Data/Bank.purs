module Data.Bank
  ( Bank
  , has
  , hasColor
  , hasPiece
  , takePiece
  ) where

import Data.ActionValidation (ActionError(..))
import Data.Either (Either(..))
import Data.Foldable (any, elem)
import Data.GamePiece (Color, GamePiece, Size, getColor, getSize)
import Data.List (List, filter)
import Data.Tuple (Tuple(..))
import Prelude (map, ($), (&&), (/=), (<<<), (==))

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
  -- let filtered = filter ((/=) gp) bank
  -- in
  if hasPiece gp bank then
    Right $ Tuple gp $ filter ((/=) gp) bank
  else
    Left $ PieceNotInBank gp
