module View.Component.Setup
  ( viewTitle
  , viewBoard
  ) where

import Data.Game (SetupState(..))
import Prelude (($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup, empty, text, (!))

viewTitle :: ∀ a. SetupState -> Markup a
viewTitle AChoosingPrimary = text "Setup: Player One Choosing First Home Star"
viewTitle (BChoosingPrimary _) = text "Setup: Player Two Choosing First Home Star"
viewTitle (AChoosingSecondary _ _) = text "Setup: Player One Choosing Second Home Star"
viewTitle (BChoosingSecondary _ _ _) = text "Setup: Player Two Choosing Second Home Star"
viewTitle _ = text "Setup: TODO"

viewBoard :: ∀ a. SetupState -> HTML a
viewBoard st =
  div ! className "game-board" $ case st of
    AChoosingPrimary -> empty
    _ -> empty
