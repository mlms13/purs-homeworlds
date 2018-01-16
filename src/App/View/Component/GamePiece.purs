module View.Component.GamePiece (view) where

import Data.GamePiece (Color, Size)
import Prelude (show, ($), (<>))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (empty, (!))

view :: ∀ a. Color -> Size -> HTML a
view color size =
  div ! className ("gp gp-color-" <> show color <> " gp-size-" <> show size) $ empty

