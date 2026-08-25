module UI.Feedback
  ( Toast
  , progress
  , spinner
  , toastRegion
  , sheet
  , raw
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)

type Toast =
  { id :: String
  , title :: String
  , description :: String
  }

progress :: forall w i. { label :: String, value :: Number, maximum :: Number } -> HH.HTML w i
progress options =
  HH.label [ css progressRoot ]
    [ HH.span_ [ HH.text options.label ]
    , HH.progress [ css bar, HP.value options.value, HP.max options.maximum ]
        [ HH.text $ show options.value <> " of " <> show options.maximum ]
    ]

spinner :: forall w i. String -> HH.HTML w i
spinner label = HH.span [ css ring, ARIA.role "status", ARIA.label label ] []

toastRegion :: forall w i. String -> Array Toast -> HH.HTML w i
toastRegion label toasts =
  HH.div [ ARIA.role "region", ARIA.label label, ARIA.live "polite" ]
    [ HH.ol [ css list ] $ toasts <#> \toast ->
        HH.li [ css toastStyle, HP.id toast.id ]
          [ HH.strong_ [ HH.text toast.title ]
          , HH.p [ css toastText ] [ HH.text toast.description ]
          ]
    ]

progressRoot :: Style
progressRoot = create
  [ "display" := "grid"
  , "gap" := var tokens.space2
  ]

bar :: Style
bar = create
  [ "accent-color" := var tokens.accent
  , "inline-size" := "100%"
  ]

ring :: Style
ring = create
  [ "animation" := "ui-spin 800ms linear infinite"
  , "block-size" := "1rem"
  , "border" := "2px solid " <> var tokens.border
  , "border-block-start-color" := var tokens.accent
  , "border-radius" := "50%"
  , "display" := "inline-block"
  , "inline-size" := "1rem"
  ]

list :: Style
list = create
  [ "display" := "grid"
  , "gap" := var tokens.space2
  , "list-style" := "none"
  , "margin" := "0"
  , "padding" := "0"
  ]

toastStyle :: Style
toastStyle = create
  [ "background-color" := var tokens.surfaceRaised
  , "border" := "1px solid " <> var tokens.border
  , "border-radius" := var tokens.radiusMd
  , "box-shadow" := var tokens.shadow
  , "padding" := "0.75rem 0.9rem"
  ]

toastText :: Style
toastText = create
  [ "color" := var tokens.textMuted
  , "font-size" := "0.875rem"
  , "margin" := "0.2rem 0 0"
  ]

sheet :: Array Style
sheet = [ progressRoot, bar, ring, list, toastStyle, toastText ]

raw :: String
raw = "@keyframes ui-spin{to{transform:rotate(360deg)}}"
