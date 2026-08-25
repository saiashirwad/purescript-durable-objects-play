module UI.Icon
  ( Icon
  , icon
  , render
  , styled
  , sheet
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Style (Style, (:=), create, css)

-- | Paths on a 24×24 grid, stroked in the current color.
newtype Icon = Icon (Array String)

icon :: Array String -> Icon
icon = Icon

render :: forall w i. Icon -> HH.HTML w i
render = styled mempty

styled :: forall w i. Style -> Icon -> HH.HTML w i
styled style (Icon paths) = svg "svg"
  [ css $ base <> style
  , attr "viewBox" "0 0 24 24"
  , attr "aria-hidden" "true"
  , attr "focusable" "false"
  ]
  (paths <#> \path -> svg "path" [ attr "d" path ] [])
  where
  svg name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)
  attr name = HP.attr (HH.AttrName name)

base :: Style
base = create
  [ "block-size" := "1rem"
  , "fill" := "none"
  , "flex" := "none"
  , "inline-size" := "1rem"
  , "stroke" := "currentColor"
  , "stroke-linecap" := "round"
  , "stroke-linejoin" := "round"
  , "stroke-width" := "1.75"
  ]

sheet :: Array Style
sheet = [ base ]
