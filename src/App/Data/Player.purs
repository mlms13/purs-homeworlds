module Data.Player where

data Player = A | B

opponent :: Player -> Player
opponent p =
  case p of
    A -> B
    B -> A
