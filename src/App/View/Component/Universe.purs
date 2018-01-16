module View.Component.Universe (view) where

import Data.StarSystem (StarSystem)
import Data.Universe (Universe)
import Prelude (($), discard)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (empty, (!))

view :: ∀ a. Universe -> HTML a
view { systems, aHome, bHome } =
  div ! className "bh-universe" $ do
    div ! className "bh-universe-home-a" $ empty
    div ! className "" $ empty
    div ! className "bh-universe-home-b" $ empty

viewHome :: ∀ a. StarSystem -> HTML a
viewHome sys =
  div empty
