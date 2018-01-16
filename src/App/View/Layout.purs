module App.View.Layout where

import App.Events (Event)
import App.State (State(..))
import App.View.Homepage as Homepage
import CSS (CSS, alignItems, backgroundColor, borderRadius, color, display, flex, fontSize, fromString, inlineBlock, key, marginTop, padding, px, rgba, value, (?))
import CSS.Common (baseline)
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, textAlign)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))
import View.CSS.GamePiece as GpCSS

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css
    Homepage.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250

  fromString "body" ? do
    backgroundColor (rgb 0 20 30)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color white
    textAlign center

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString "a" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration

  fromString ".bh-bank-color-row" ? do
    display flex
    alignItems baseline

  fromString ".bh-bank-piece" ? do
    key (fromString "cursor") (value "pointer")
    borderRadius (3.0 #px) (3.0 #px) (3.0 #px) (3.0 #px)
    padding (15.0 #px) (15.0 #px) (15.0 #px) (15.0 #px)
    key (fromString "transition") (value "0.3s background-color")

  fromString ".bh-bank-piece:hover" ? do
    backgroundColor $ rgba 255 255 255 0.2

  fromString ".bh-bank-count" ? do
    marginTop (4.0 #px)

  GpCSS.css
