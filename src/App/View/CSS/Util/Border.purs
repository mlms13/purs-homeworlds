module View.CSS.Util.Border
  ( borderStyle
  , borderWidth
  , borderWidthVH
  , borderTopColor
  , borderRightColor
  , borderBottomColor
  , borderLeftColor
  ) where

import CSS (Abs, CSS, Size, Stroke, fromString, key, value)
import Color (Color)
import Data.Tuple.Nested (tuple2)
import Prelude ((<<<))

borderTopColor :: Color -> CSS
borderTopColor = key (fromString "border-top-color") <<< value

borderRightColor :: Color -> CSS
borderRightColor = key (fromString "border-right-color") <<< value

borderBottomColor :: Color -> CSS
borderBottomColor = key (fromString "border-bottom-color") <<< value

borderLeftColor :: Color -> CSS
borderLeftColor = key (fromString "border-left-color") <<< value

borderStyle :: Stroke -> CSS
borderStyle = key (fromString "border-style") <<< value

borderWidth :: Size Abs -> CSS
borderWidth = key (fromString "border-width") <<< value

borderWidthVH :: Size Abs -> Size Abs -> CSS
borderWidthVH v h = key (fromString "border-width") (tuple2 v h)
