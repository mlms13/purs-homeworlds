module View.Component.Game (view) where

import App.Events (Event)
import Data.Game (Game(..))
import Prelude (($), discard)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h3)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (empty, (!))
import View.Component.Bank as VBank
import View.Component.Setup as VSetup

view :: Game -> HTML Event
view game =
  div ! className "bh-game" $
    case game of
      Setup bank st -> div do
        h3 $ VSetup.viewTitle st
        VBank.view bank
        VSetup.viewBoard st
      _ -> div empty

