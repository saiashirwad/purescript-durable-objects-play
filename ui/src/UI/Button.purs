module UI.Button
  ( Options
  , defaults
  , button
  , submit
  , iconButton
  , sheet
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Core (Size(..), Tone(..), bool, dataAttr, sizeName, toneName)
import UI.Style (Style, (:=), create, css, on, prefer, var)
import UI.Style as Style
import UI.Theme (tokens)

type Options =
  { tone :: Tone
  , size :: Size
  , disabled :: Boolean
  , busy :: Boolean
  , styles :: Style
  }

defaults :: Options
defaults =
  { tone: Neutral
  , size: Medium
  , disabled: false
  , busy: false
  , styles: mempty
  }

button :: forall w i. Options -> HH.Node I.HTMLbutton w i
button = render HP.ButtonButton mempty

submit :: forall w i. Options -> HH.Node I.HTMLbutton w i
submit = render HP.ButtonSubmit mempty

-- | A round button whose only content is an icon; `label` names it.
iconButton :: forall w i. String -> Options -> HH.Node I.HTMLbutton w i
iconButton label options properties = render HP.ButtonButton icon options $ [ ARIA.label label ] <> properties

render :: forall w i. HP.ButtonType -> Style -> Options -> HH.Node I.HTMLbutton w i
render kind extra options properties = HH.button $
  [ css $ base <> sized options.size <> toned options.tone <> extra <> options.styles
  , HP.type_ kind
  , HP.disabled (options.disabled || options.busy)
  , ARIA.busy (bool options.busy)
  , dataAttr "ui" "button"
  , dataAttr "tone" (toneName options.tone)
  , dataAttr "size" (sizeName options.size)
  ] <> properties

base :: Style
base = create
  [ "align-items" := "center"
  , "appearance" := "none"
  , "border" := "1px solid transparent"
  , "border-radius" := var tokens.radiusMd
  , "cursor" := "pointer"
  , "display" := "inline-flex"
  , "font" := "inherit"
  , "font-weight" := "500"
  , "gap" := var tokens.space2
  , "justify-content" := "center"
  , "letter-spacing" := "-0.005em"
  , "line-height" := "1.2"
  , "min-block-size" := "2.5rem"
  , "text-decoration" := "none"
  , "transition" := "background-color 120ms ease,border-color 120ms ease,color 120ms ease,box-shadow 120ms ease,transform 120ms ease"
  , "user-select" := "none"
  , "white-space" := "nowrap"
  , on Style.FocusVisible
      [ "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "2px"
      ]
  , on Style.Active [ "transform" := "translateY(1px)" ]
  , on Style.Disabled
      [ "cursor" := "not-allowed"
      , "opacity" := "0.5"
      , "transform" := "none"
      ]
  , prefer Style.ReducedMotion
      [ "transition-duration" := "0.01ms"
      , "transform" := "none"
      ]
  ]

sized :: Size -> Style
sized = case _ of
  Small -> small
  Medium -> medium
  Large -> large

toned :: Tone -> Style
toned = case _ of
  Neutral -> neutral
  Accent -> accent
  Danger -> danger
  Quiet -> quiet

small :: Style
small = create
  [ "font-size" := "0.8125rem"
  , "min-block-size" := "2rem"
  , "padding-block" := "0.3rem"
  , "padding-inline" := "0.7rem"
  ]

medium :: Style
medium = create
  [ "font-size" := "0.9375rem"
  , "padding-block" := "0.55rem"
  , "padding-inline" := "1rem"
  ]

large :: Style
large = create
  [ "font-size" := "1rem"
  , "min-block-size" := "3rem"
  , "padding-block" := "0.7rem"
  , "padding-inline" := "1.25rem"
  ]

neutral :: Style
neutral = create
  [ "background-color" := var tokens.surface
  , "border-color" := var tokens.border
  , "box-shadow" := "0 1px 2px oklch(0% 0 0 / 4%)"
  , "color" := var tokens.text
  , on Style.Hover [ "background-color" := var tokens.background ]
  ]

accent :: Style
accent = create
  [ "background-color" := var tokens.accent
  , "box-shadow" := "inset 0 1px 0 oklch(100% 0 0 / 18%),0 1px 2px oklch(0% 0 0 / 10%)"
  , "color" := var tokens.accentText
  , on Style.Hover [ "background-color" := var tokens.accentHover ]
  ]

danger :: Style
danger = create
  [ "background-color" := var tokens.danger
  , "color" := var tokens.dangerText
  ]

quiet :: Style
quiet = create
  [ "background-color" := "transparent"
  , "color" := var tokens.textMuted
  , on Style.Hover
      [ "background-color" := var tokens.tint
      , "color" := var tokens.text
      ]
  ]

icon :: Style
icon = create
  [ "aspect-ratio" := "1"
  , "border-radius" := "999px"
  , "padding-block" := "0.5rem"
  , "padding-inline" := "0.5rem"
  ]

sheet :: Array Style
sheet = [ base, small, medium, large, neutral, accent, danger, quiet, icon ]
