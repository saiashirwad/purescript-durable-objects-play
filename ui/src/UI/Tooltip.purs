module UI.Tooltip
  ( iconButton
  , sheet
  , raw
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Button as Button
import UI.Core (dataAttr)
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)

-- | An icon button whose label also shows on hover and focus.
iconButton
  :: forall w i
   . { id :: String, label :: String, button :: Button.Options }
  -> Array (HH.IProp I.HTMLbutton i)
  -> Array (HH.HTML w i)
  -> HH.HTML w i
iconButton options properties children =
  HH.span [ css root, dataAttr "ui" "tooltip-root" ]
    [ Button.iconButton options.label options.button ([ ARIA.describedBy options.id ] <> properties) children
    , HH.span [ css popup, HP.id options.id, ARIA.role "tooltip", dataAttr "ui" "tooltip" ] [ HH.text options.label ]
    ]

root :: Style
root = create
  [ "display" := "inline-flex"
  , "position" := "relative"
  ]

popup :: Style
popup = create
  [ "background-color" := var tokens.text
  , "border-radius" := var tokens.radiusSm
  , "color" := var tokens.background
  , "font-size" := "0.75rem"
  , "font-weight" := "500"
  , "inset-block-end" := "calc(100% + 0.4rem)"
  , "inset-inline-start" := "50%"
  , "opacity" := "0"
  , "padding-block" := "0.3rem"
  , "padding-inline" := "0.5rem"
  , "pointer-events" := "none"
  , "position" := "absolute"
  , "transform" := "translateX(-50%) translateY(0.2rem)"
  , "transition" := "opacity 100ms ease,transform 100ms ease"
  , "visibility" := "hidden"
  , "white-space" := "nowrap"
  , "z-index" := "20"
  ]

sheet :: Array Style
sheet = [ root, popup ]

raw :: String
raw = "[data-ui=tooltip-root]:hover [data-ui=tooltip],[data-ui=tooltip-root]:focus-within [data-ui=tooltip]{opacity:1;transform:translateX(-50%);visibility:visible}"
