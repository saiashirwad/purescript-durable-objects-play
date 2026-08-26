module UI.Select
  ( select
  , sheet
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Data.Maybe (isJust)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Core (Choice, bool)
import UI.Field as Field
import UI.Input as Input
import UI.Style (Style, (:=), create, css)

-- | A native select inside a `Field`.
select :: forall w i. Field.Options -> Array Choice -> Array (HH.IProp I.HTMLselect i) -> HH.HTML w i
select options choices properties = Field.field options $ HH.select
  ( [ css $ control <> options.styles
    , HP.id options.id
    , HP.disabled options.disabled
    , HP.required options.required
    , ARIA.invalid $ bool $ isJust options.error
    , Field.describedBy options
    ] <> properties
  )
  (choices <#> \choice -> HH.option [ HP.value choice.value, HP.disabled choice.disabled ] [ HH.text choice.label ])

-- | An input, but the browser keeps its arrow.
control :: Style
control = Input.control <> create
  [ "appearance" := "auto"
  , "padding-block" := "0"
  ]

sheet :: Array Style
sheet = [ control ]
