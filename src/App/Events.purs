module App.Events where

import Data.Game

import App.State (State(..))
import Data.Bank (Bank, takePiece)
import Data.Either (Either(..))
import Data.Function (($))
import Data.GamePiece (Color, GamePiece, Ship, Star)
import Data.List (List(..))
import Data.Player (Player(..))
import Data.StarSystem (StarSystem(..), homeworld)
import Data.Tuple (Tuple(..))
import Data.Universe (universe)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event
  = ChoosePiece GamePiece
  | Action StarSystem GameAction

data GameAction
  = Construct Color
  | Trade Ship Color
  | Capture Ship
  | Move Ship MoveAction
  | Sacrifice Ship

data MoveAction
  = ToExisting StarSystem
  | ToNew Star

type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp msg (State st) = noEffects $
  case (Tuple msg st.game) of
    Tuple (ChoosePiece p) (Setup bank state) ->
      State $ st { game = choosePieceInSetup p bank state }
    _ -> State st


choosePieceInSetup :: GamePiece -> Bank -> SetupState -> Game
choosePieceInSetup gp bank state =
  case takePiece gp bank of
    Left _ ->
      Setup bank state -- TODO: noop is ok, but we could notify here

    Right (Tuple piece updated) ->
      let setup = Setup updated
      in case state of
        AChoosingPrimary ->
          setup $ BChoosingPrimary $ APrimary piece
        BChoosingPrimary a ->
          setup $ AChoosingSecondary a $ BPrimary piece
        AChoosingSecondary a b ->
          setup $ BChoosingSecondary a b $ ASecondary piece
        BChoosingSecondary a b a2 ->
          setup $ AChoosingShip a b a2 $ BSecondary piece
        AChoosingShip (APrimary a) b (ASecondary a2) b2 ->
          setup $ BChoosingShip (homeworld A a a2 piece) b b2
        BChoosingShip a (BPrimary b) (BSecondary b2) ->
          Playing updated $ playingState (Tuple A AnySingle)
            $ universe Nil (Binary a) (Binary $ homeworld B b b2 piece)
