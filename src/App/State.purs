module App.State where

import Data.Game (Game(..), SetupState(..))
import Data.GamePiece (all)
import Data.Newtype (class Newtype)

newtype State = State
  { game :: Game
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { game: Setup all AChoosingPrimary
  }
