module UI.Status
  ( polite
  , assertive
  , error
  , visuallyHidden
  , sheet
  ) where

import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)

polite :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
polite = HH.div [ ARIA.role "status", ARIA.live "polite", ARIA.atomic "true" ]

assertive :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
assertive = HH.div [ ARIA.role "alert", ARIA.live "assertive", ARIA.atomic "true" ]

error :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
error = HH.p [ css errorStyle, ARIA.role "alert" ]

-- | Read by screen readers, drawn nowhere.
visuallyHidden :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
visuallyHidden = HH.span [ css hidden ]

errorStyle :: Style
errorStyle = create
  [ "color" := var tokens.danger
  , "font-size" := "0.875rem"
  , "margin" := "0"
  ]

hidden :: Style
hidden = create
  [ "block-size" := "1px"
  , "border" := "0"
  , "clip" := "rect(0 0 0 0)"
  , "clip-path" := "inset(50%)"
  , "inline-size" := "1px"
  , "margin" := "-1px"
  , "overflow" := "hidden"
  , "padding" := "0"
  , "position" := "absolute"
  , "white-space" := "nowrap"
  ]

sheet :: Array Style
sheet = [ errorStyle, hidden ]
