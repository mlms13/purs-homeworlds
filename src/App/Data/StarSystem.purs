module Data.StarSystem where

import ActionValidation (ActionError(..), ActionValidation(..))
import Data.GamePiece (GamePiece, Star, Ship, Color, getColor)
import Data.List (List(Nil), elem, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.NonEmpty (NonEmpty(..), fromNonEmpty, oneOf, singleton)
import Data.Player (Player(..))
import Data.Tuple (Tuple(..))
import Prelude (append, map, ($), (<<<))

data StarSystem
  = Single SingleSystem
  | Binary BinarySystem

data ShipsAtStar
  = PlayerA (NonEmpty List Ship)
  | PlayerB (NonEmpty List Ship)
  | AAndB (NonEmpty List Ship) (NonEmpty List Ship)

type SingleSystem =
  { star :: GamePiece
  , ships :: ShipsAtStar
  }

type BinarySystem =
  { primary :: Star
  , secondary :: Star
  , ships :: ShipsAtStar
  }

singleSystem :: Star -> ShipsAtStar -> SingleSystem
singleSystem star ships =
  { star, ships }

binarySystem :: Star -> Star -> ShipsAtStar -> BinarySystem
binarySystem primary secondary ships =
  { primary, secondary, ships }

-- Create a brand new homeworld for a player
homeworld :: Player -> Star -> Star -> Ship -> BinarySystem
homeworld player a b ship =
  let
    ships :: ShipsAtStar
    ships = case player of
      A -> PlayerA $ singleton ship
      B -> PlayerB $ singleton ship
  in
  binarySystem a b ships


systemShips :: StarSystem -> ShipsAtStar
systemShips (Single { ships }) = ships
systemShips (Binary { ships }) = ships

systemStarColors :: StarSystem -> NonEmpty List Color
systemStarColors (Single { star }) = singleton $ getColor star
systemStarColors (Binary { primary, secondary }) = NonEmpty (getColor primary) (getColor secondary : Nil)

-----------------------------------
-- rules enforcement and validation
-----------------------------------

playerShipsAtSystem :: Player -> StarSystem -> Maybe (NonEmpty List Ship)
playerShipsAtSystem player sys =
  case (Tuple player $ systemShips sys) of
    Tuple A (PlayerA v) -> Just v
    Tuple B (PlayerB v) -> Just v
    Tuple A (AAndB v _) -> Just v
    Tuple B (AAndB _ v) -> Just v
    Tuple _ _ -> Nothing

playerColorsAtSystem :: Player -> StarSystem -> List Color
playerColorsAtSystem player sys =
  let
    starColors = fromNonEmpty (:) $ systemStarColors sys

    shipColors :: Maybe (NonEmpty List Color)
    shipColors = map (map getColor) $ playerShipsAtSystem player sys
  in
  maybe Nil (append starColors <<< oneOf) shipColors

validateColorAccess :: Player -> StarSystem -> Color -> ActionValidation
validateColorAccess player sys c =
  let ok = elem c $ playerColorsAtSystem player sys
  in if ok then Success else Error $ singleton $ IllegalColorAction c

validateShipColorAccess :: Player -> StarSystem -> Color -> ActionValidation
validateShipColorAccess player sys c =
  let ok = maybe false (elem c <<< map getColor <<< fromNonEmpty (:)) $ playerShipsAtSystem player sys
  in if ok then Success else Error $ singleton $ NoShipOfColorAtSystem c

-- does the player control this ship at this star system?
hasShip :: Player -> Ship -> StarSystem -> ActionValidation
hasShip player ship sys =
  let
    ships = maybe Nil (fromNonEmpty (:)) $ playerShipsAtSystem player sys
    ok = elem ship ships
  in if ok then Success else Error $ singleton $ NoShipAtSystem ship

