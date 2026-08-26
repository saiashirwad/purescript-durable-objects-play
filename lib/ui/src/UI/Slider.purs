module UI.Slider
  ( Options
  , slider
  , sheet
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)

type Options action =
  { id :: String
  , label :: String
  , value :: Number
  , minimum :: Number
  , maximum :: Number
  , step :: Number
  , disabled :: Boolean
  , onChange :: String -> action
  }

-- | A native range input with its value shown beside the label.
slider :: forall w action. Options action -> HH.HTML w action
slider options =
  HH.label [ css root, HP.for options.id ]
    [ HH.span [ css heading ]
        [ HH.span_ [ HH.text options.label ]
        , HH.output [ css value, HP.for options.id ] [ HH.text $ show options.value ]
        ]
    , HH.input
        [ css range
        , HP.id options.id
        , HP.type_ HP.InputRange
        , HP.value $ show options.value
        , HP.disabled options.disabled
        , HP.attr (HH.AttrName "min") $ show options.minimum
        , HP.attr (HH.AttrName "max") $ show options.maximum
        , HP.attr (HH.AttrName "step") $ show options.step
        , ARIA.valueMin $ show options.minimum
        , ARIA.valueMax $ show options.maximum
        , ARIA.valueNow $ show options.value
        , HE.onValueInput options.onChange
        ]
    ]

root :: Style
root = create
  [ "display" := "grid"
  , "gap" := var tokens.space2
  ]

heading :: Style
heading = create
  [ "display" := "flex"
  , "font-size" := "0.875rem"
  , "font-weight" := "500"
  , "justify-content" := "space-between"
  ]

value :: Style
value = create
  [ "color" := var tokens.textMuted
  , "font-variant-numeric" := "tabular-nums"
  ]

range :: Style
range = create
  [ "accent-color" := var tokens.accent
  , "cursor" := "pointer"
  , "inline-size" := "100%"
  , on Style.FocusVisible
      [ "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "3px"
      ]
  , on Style.Disabled
      [ "cursor" := "not-allowed"
      , "opacity" := "0.5"
      ]
  ]

sheet :: Array Style
sheet = [ root, heading, value, range ]
