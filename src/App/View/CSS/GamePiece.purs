module View.CSS.GamePiece (css) where

import CSS (CSS, absolute, bottom, darken, fromString, height, key, left, pct, position, px, relative, right, solid, value, width, (?))
import Prelude (discard, ($), (<<<))
import View.CSS.Util.Border (borderBottomColor, borderLeftColor, borderRightColor, borderStyle, borderTopColor, borderWidthVH)
import View.CSS.Util.Colors (transparent, green, yellow, blue, red)

css :: CSS
css = do
  let
    content :: String -> CSS -- TODO: wrap string in literal quote characters
    content = key (fromString "content") <<< value

  fromString ".gp" ? do
    position relative

  fromString ".gp::before, .gp::after" ? do
    content "\"\""
    borderStyle solid
    position absolute
    bottom $ px 0.0

  fromString ".gp::before" ? do
    borderTopColor transparent
    borderLeftColor transparent
    right $ pct 50.0

  fromString ".gp::after" ? do
    borderTopColor transparent
    borderRightColor transparent
    left $ pct 50.0


  -------------------------------------------------------------------------------
  -- Sizes
  -------------------------------------------------------------------------------

  fromString ".gp-size-large" ? do
    width $ px 80.0
    height $ px 80.0

  fromString ".gp-size-large::before, .gp-size-large::after" ? do
    borderWidthVH (px 40.0) (px 20.0)

  fromString ".gp-size-medium" ? do
    width $ px 60.0
    height $ px 60.0

  fromString ".gp-size-medium::before, .gp-size-medium::after" ? do
    borderWidthVH (px 30.0) (px 15.0)

  fromString ".gp-size-small" ? do
    width $ px 40.0
    height $ px 40.0

  fromString ".gp-size-small::before, .gp-size-small::after" ? do
    borderWidthVH (px 20.0) (px 10.0)

  -------------------------------------------------------------------------------
  -- Colors
  -------------------------------------------------------------------------------

  fromString ".gp-color-green::before" ? do
    borderBottomColor green
    borderRightColor green

  fromString ".gp-color-green::after" ? do
    borderBottomColor $ darken 0.07 green
    borderLeftColor $ darken 0.07 green

  fromString ".gp-color-yellow::before" ? do
    borderBottomColor yellow
    borderRightColor yellow

  fromString ".gp-color-yellow::after" ? do
    borderBottomColor $ darken 0.07 yellow
    borderLeftColor $ darken 0.07 yellow

  fromString ".gp-color-red::before" ? do
    borderBottomColor red
    borderRightColor red

  fromString ".gp-color-red::after" ? do
    borderBottomColor $ darken 0.07 red
    borderLeftColor $ darken 0.07 red

  fromString ".gp-color-blue::before" ? do
    borderBottomColor blue
    borderRightColor blue

  fromString ".gp-color-blue::after" ? do
    borderBottomColor $ darken 0.07 blue
    borderLeftColor $ darken 0.07 blue
