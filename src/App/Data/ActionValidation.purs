module ActionValidation where

import Data.GamePiece (Color, GamePiece, Size, Ship)
import Data.List (List, singleton)
import Data.NonEmpty (NonEmpty(..))
import Prelude (class Semigroup, ($), (<>))

data ActionValidation
  = Success
  | Error (NonEmpty List ActionError)

data ActionError
  = PieceNotInBank GamePiece
  | SizeColorNotInBank Size Color
  | IllegalColorAction Color
  | IllegalTargetPiece Ship
  | NoShipOfColorAtSystem Color
  | NoShipAtSystem Ship

instance actionValidationSemigroup :: Semigroup ActionValidation where
  append :: ActionValidation -> ActionValidation -> ActionValidation
  append Success Success = Success
  append Success (Error e) = Error e
  append (Error e) Success = Error e
  append (Error (NonEmpty ea lista)) (Error (NonEmpty eb listb)) =
    Error $ NonEmpty ea $ lista <> singleton eb <> listb

