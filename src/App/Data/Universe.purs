module Data.Universe
  ( Universe
  , universe
  , hasSystem
  ) where

import Data.Foldable (elem)
import Data.List (List)
import Data.StarSystem (StarSystem, SingleSystem)

type Universe =
  { systems :: List SingleSystem
  , aHome :: StarSystem
  , bHome :: StarSystem
  }

universe :: List SingleSystem -> StarSystem -> StarSystem -> Universe
universe systems a b =
  { systems : systems
  , aHome : a
  , bHome : b
  }

hasSystem :: SingleSystem -> Universe -> Boolean
hasSystem sys { systems } = elem sys systems
