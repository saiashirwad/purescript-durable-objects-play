module UI.Checkbox
  ( Options
  , checkbox
  , switch
  , sheet
  , raw
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Data.Monoid (guard)
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Core (bool, dataAttr)
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)

type Options =
  { id :: String
  , label :: String
  , checked :: Boolean
  , disabled :: Boolean
  }

checkbox :: forall w i. Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
checkbox = control false

switch :: forall w i. Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
switch = control true

-- | The native input stays for keyboard and screen readers; the box beside
-- | it is what you see. `raw` styles the box from the input's state.
control :: forall w i. Boolean -> Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
control isSwitch options properties =
  HH.label [ css root, HP.for options.id, dataAttr "ui" if isSwitch then "switch" else "checkbox" ]
    [ HH.input $
        [ css native
        , HP.id options.id
        , HP.type_ HP.InputCheckbox
        , HP.checked options.checked
        , HP.disabled options.disabled
        , ARIA.checked $ bool options.checked
        ] <> guard isSwitch [ ARIA.role "switch" ] <> properties
    , HH.span [ css $ box <> if isSwitch then track else square, ARIA.hidden "true" ] []
    , HH.span [ css labelStyle ] [ HH.text options.label ]
    ]

root :: Style
root = create
  [ "align-items" := "center"
  , "color" := var tokens.text
  , "cursor" := "pointer"
  , "display" := "inline-flex"
  , "gap" := var tokens.space2
  , "position" := "relative"
  ]

native :: Style
native = create
  [ "block-size" := "1px"
  , "clip" := "rect(0 0 0 0)"
  , "inline-size" := "1px"
  , "opacity" := "0"
  , "overflow" := "hidden"
  , "position" := "absolute"
  ]

box :: Style
box = create
  [ "background-color" := var tokens.surface
  , "border" := "1px solid " <> var tokens.border
  , "box-shadow" := "inset 0 1px 2px oklch(0% 0 0 / 6%)"
  , "display" := "inline-block"
  , "flex" := "none"
  , "transition" := "background-color 120ms ease,border-color 120ms ease,box-shadow 120ms ease"
  ]

square :: Style
square = create
  [ "block-size" := "1.125rem"
  , "border-radius" := "0.375rem"
  , "inline-size" := "1.125rem"
  ]

track :: Style
track = create
  [ "block-size" := "1.375rem"
  , "border-radius" := "999px"
  , "inline-size" := "2.375rem"
  ]

labelStyle :: Style
labelStyle = create
  [ "font-size" := "0.9375rem"
  , "font-weight" := "500"
  ]

sheet :: Array Style
sheet = [ root, native, box, square, track, labelStyle ]

-- | What the atoms cannot say: the box reacts to its sibling input.
raw :: String
raw = joinWith ""
  [ both "input:focus-visible+[aria-hidden]" $ "outline:2px solid " <> var tokens.focusRing <> ";outline-offset:2px"
  , both "input:checked+[aria-hidden]" $ "background-color:" <> var tokens.accent <> ";border-color:" <> var tokens.accent
  , "[data-ui=checkbox] input:checked+[aria-hidden]::after{color:" <> var tokens.accentText <> ";content:'✓';display:grid;font-size:.8rem;font-weight:700;place-items:center}"
  , "[data-ui=switch] [aria-hidden]::after{background:" <> var tokens.textMuted <> ";block-size:1rem;border-radius:50%;content:'';display:block;inline-size:1rem;margin:.125rem;transition:transform 140ms ease,background-color 140ms ease}"
  , "[data-ui=switch] input:checked+[aria-hidden]::after{background:" <> var tokens.accentText <> ";transform:translateX(1rem)}"
  ]
  where
  both selector body = "[data-ui=checkbox] " <> selector <> ",[data-ui=switch] " <> selector <> "{" <> body <> "}"
