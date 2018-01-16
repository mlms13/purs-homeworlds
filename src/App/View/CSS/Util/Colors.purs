module View.CSS.Util.Colors
  ( transparent
  , green
  , yellow
  , blue
  , red
  ) where

import Color (Color, rgba)

transparent :: Color
transparent = rgba 0 0 0 0.0

green :: Color
green = rgba 30 220 90 1.0

blue :: Color
blue = rgba 30 90 220 1.0

yellow :: Color
yellow = rgba 240 220 40 1.0

red :: Color
red = rgba 220 30 90 1.0
