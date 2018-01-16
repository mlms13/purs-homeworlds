module View.Component.Bank (view) where

import App.Events (Event(..))
import Data.Bank (Bank)
import Data.Foldable (foldl)
import Data.GamePiece (Color(..), GamePiece(..), Size(..), gamepiece)
import Prelude (const, discard, show, ($), (+))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!), (#!))
import View.Component.GamePiece as Piece

type GroupedPieces =
  { green :: { large :: Int, med :: Int, small :: Int }
  , yellow :: { large :: Int, med :: Int, small :: Int }
  , blue :: { large :: Int, med :: Int, small :: Int }
  , red :: { large :: Int, med :: Int, small :: Int }
  }

groupEmpty :: GroupedPieces
groupEmpty =
  { green : { large : 0, med : 0, small : 0 }
  , yellow : { large : 0, med : 0, small : 0 }
  , blue : { large : 0, med : 0, small : 0 }
  , red : { large : 0, med : 0, small : 0 }
  }

groupPieces :: Bank -> GroupedPieces
groupPieces bank =
  foldl group groupEmpty bank
  where
    group :: GroupedPieces -> GamePiece -> GroupedPieces
    group { green, yellow, blue, red } (GamePiece { color, size }) =
      case color of
        Green -> { yellow, blue, red, green : case size of
                   Large -> green { large = green.large + 1 }
                   Medium -> green { med = green.med + 1 }
                   Small -> green { small = green.small + 1 }
                 }
        Yellow -> { blue, red, green, yellow : case size of
                    Large -> yellow { large = yellow.large + 1 }
                    Medium -> yellow { med = yellow.med + 1 }
                    Small -> yellow { small = yellow.small + 1 }
                  }
        Blue -> { red, green, yellow, blue : case size of
                  Large -> blue { large = blue.large + 1 }
                  Medium -> blue { med = blue.med + 1 }
                  Small -> blue { small = blue.small + 1 }
                }
        Red -> { green, yellow, blue, red : case size of
                 Large -> red { large = red.large + 1 }
                 Medium -> red { med = red.med + 1 }
                 Small -> red { small = red.small + 1 }
               }

view :: Bank -> HTML Event
view bank =
  div ! className "bh-bank" $ viewGrouped (groupPieces bank)

viewGrouped :: GroupedPieces -> HTML Event
viewGrouped { green, yellow, blue, red} =
  do
    div ! className "bh-bank-color-row" $ do
      viewBankPiece green.large Green Large
      viewBankPiece green.med Green Medium
      viewBankPiece green.small Green Small
    div ! className "bh-bank-color-row" $ do
      viewBankPiece yellow.large Yellow Large
      viewBankPiece yellow.med Yellow Medium
      viewBankPiece yellow.small Yellow Small
    div ! className "bh-bank-color-row" $ do
      viewBankPiece blue.large Blue Large
      viewBankPiece blue.med Blue Medium
      viewBankPiece blue.small Blue Small
    div ! className "bh-bank-color-row" $ do
      viewBankPiece red.large Red Large
      viewBankPiece red.med Red Medium
      viewBankPiece red.small Red Small

viewBankPiece :: Int -> Color -> Size -> HTML Event
viewBankPiece count color size =
  div
    ! className "bh-bank-piece"
    #! onClick (const $ ChoosePiece $ gamepiece color size)
    $ do
      Piece.view color size
      span ! className "bh-bank-count" $ text (show count)
