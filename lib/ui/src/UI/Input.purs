module UI.Input
  ( Options
  , defaults
  , text
  , password
  , control
  , sheet
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Core (bool)
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)

type Options =
  { disabled :: Boolean
  , invalid :: Boolean
  , styles :: Style
  }

defaults :: Options
defaults =
  { disabled: false
  , invalid: false
  , styles: mempty
  }

text :: forall w i. Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
text = render HP.InputText

password :: forall w i. Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
password = render HP.InputPassword

render :: forall w i. HP.InputType -> Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
render kind options properties = HH.input $
  [ css $ control <> options.styles
  , HP.type_ kind
  , HP.disabled options.disabled
  , ARIA.invalid $ bool options.invalid
  ] <> properties

-- | The look of any text control; `Field.textarea` and `Select` build on it.
control :: Style
control = create
  [ "appearance" := "none"
  , "background-color" := var tokens.surface
  , "border" := "1px solid " <> var tokens.border
  , "border-radius" := var tokens.radiusMd
  , "color" := var tokens.text
  , "font" := "inherit"
  , "inline-size" := "100%"
  , "min-block-size" := "2.75rem"
  , "padding-block" := "0.6rem"
  , "padding-inline" := "0.85rem"
  , "transition" := "border-color 120ms ease,box-shadow 120ms ease"
  , on Style.FocusVisible
      [ "border-color" := var tokens.accent
      , "box-shadow" := "0 0 0 3px " <> var tokens.accentSoft
      , "outline" := "2px solid transparent"
      ]
  , on Style.Invalid [ "border-color" := var tokens.danger ]
  , on Style.Disabled
      [ "cursor" := "not-allowed"
      , "opacity" := "0.55"
      ]
  ]

sheet :: Array Style
sheet = [ control ]
