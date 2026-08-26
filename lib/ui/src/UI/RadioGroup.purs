module UI.RadioGroup
  ( Options
  , radioGroup
  , sheet
  ) where

import Prelude

import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import UI.Core (Choice)
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)

type Options action =
  { id :: String
  , name :: String
  , label :: String
  , description :: Maybe String
  , value :: String
  , onChange :: String -> action
  }

-- | Native radios in a fieldset; arrow keys come for free.
radioGroup :: forall w action. Options action -> Array Choice -> HH.HTML w action
radioGroup options choices = HH.fieldset [ css root, HP.id options.id ] $ fold
  [ [ HH.legend [ css legend ] [ HH.text options.label ] ]
  , foldMap (\text -> [ HH.p [ css description ] [ HH.text text ] ]) options.description
  , choices <#> \choice -> HH.label [ css option ]
      [ HH.input
          [ css radio
          , HP.type_ HP.InputRadio
          , HP.name options.name
          , HP.value choice.value
          , HP.checked $ choice.value == options.value
          , HP.disabled choice.disabled
          , HE.onClick \_ -> options.onChange choice.value
          ]
      , HH.span_ [ HH.text choice.label ]
      ]
  ]

root :: Style
root = create
  [ "border" := "0"
  , "display" := "grid"
  , "gap" := var tokens.space2
  , "margin" := "0"
  , "padding" := "0"
  ]

legend :: Style
legend = create
  [ "font-size" := "0.875rem"
  , "font-weight" := "500"
  , "padding" := "0"
  ]

description :: Style
description = create
  [ "color" := var tokens.textMuted
  , "font-size" := "0.8125rem"
  , "margin" := "0"
  ]

option :: Style
option = create
  [ "align-items" := "center"
  , "cursor" := "pointer"
  , "display" := "flex"
  , "gap" := var tokens.space2
  ]

radio :: Style
radio = create
  [ "accent-color" := var tokens.accent
  , "block-size" := "1.125rem"
  , "inline-size" := "1.125rem"
  , "margin" := "0"
  , on Style.FocusVisible
      [ "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "2px"
      ]
  ]

sheet :: Array Style
sheet = [ root, legend, description, option, radio ]
