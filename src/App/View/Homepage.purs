module App.View.Homepage where

import App.Events (Event)
import App.State (State(..))
import Pux.DOM.HTML (HTML)
import View.Component.Game as VGame

view :: State -> HTML Event
view (State s) =
  VGame.view s.game
