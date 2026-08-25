module UI.Avatar
  ( Options
  , avatar
  , hue
  , sheet
  ) where

import Prelude

import Data.Array as Array
import Data.Enum (fromEnum)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Style (Style, (:=), create, css, var)
import UI.Style as Style

type Options =
  { fallback :: String
  , hue :: Int
  , styles :: Style
  }

avatar :: forall w i. Options -> HH.HTML w i
avatar options = HH.span
  [ css $ base <> options.styles
  , Style.inlineVars [ hueVariable /\ show options.hue ]
  , ARIA.hidden "true"
  ]
  [ HH.text options.fallback ]

-- | A stable hue for a name.
hue :: String -> Int
hue = String.toCodePointArray >>> map fromEnum >>> Array.foldl (\value point -> (value * 31 + point) `mod` 360) 7

hueVariable :: Style.Var
hueVariable = Style.variable "avatar-hue"

base :: Style
base = create
  [ "align-items" := "center"
  , "background" := "linear-gradient(145deg,oklch(72% 0.14 " <> var hueVariable <> "),oklch(56% 0.16 calc(" <> var hueVariable <> " + 40)))"
  , "block-size" := "1.75rem"
  , "border-radius" := "50%"
  , "box-shadow" := "inset 0 0 0 1px oklch(100% 0 0 / 22%)"
  , "color" := "oklch(100% 0 0)"
  , "display" := "inline-flex"
  , "flex" := "none"
  , "font-size" := "0.7rem"
  , "font-weight" := "600"
  , "inline-size" := "1.75rem"
  , "justify-content" := "center"
  , "letter-spacing" := "0.02em"
  ]

sheet :: Array Style
sheet = [ base ]
