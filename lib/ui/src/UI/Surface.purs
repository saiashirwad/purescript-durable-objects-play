module UI.Surface
  ( card
  , badge
  , separator
  , stack
  , cluster
  , sheet
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)

card :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
card = HH.section [ css cardStyle ]

badge :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
badge = HH.span [ css badgeStyle ]

separator :: forall w i. HH.HTML w i
separator = HH.hr [ css separatorStyle, ARIA.hidden "true" ]

-- | Children in a column.
stack :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
stack = HH.div [ css stackStyle ]

-- | Children in a wrapping row.
cluster :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
cluster = HH.div [ css clusterStyle ]

cardStyle :: Style
cardStyle = create
  [ "background-color" := var tokens.surface
  , "border" := "1px solid " <> var tokens.border
  , "border-radius" := var tokens.radiusLg
  , "box-shadow" := var tokens.shadow
  , "padding" := "1.5rem"
  ]

badgeStyle :: Style
badgeStyle = create
  [ "align-items" := "center"
  , "background-color" := var tokens.tint
  , "border-radius" := "999px"
  , "color" := var tokens.textMuted
  , "display" := "inline-flex"
  , "font-size" := "0.75rem"
  , "font-weight" := "500"
  , "padding-block" := "0.2rem"
  , "padding-inline" := "0.6rem"
  ]

separatorStyle :: Style
separatorStyle = create
  [ "background-color" := var tokens.border
  , "block-size" := "1px"
  , "border" := "0"
  , "inline-size" := "100%"
  , "margin-block" := "1rem"
  ]

stackStyle :: Style
stackStyle = create
  [ "display" := "flex"
  , "flex-direction" := "column"
  , "gap" := var tokens.space3
  ]

clusterStyle :: Style
clusterStyle = create
  [ "align-items" := "center"
  , "display" := "flex"
  , "flex-wrap" := "wrap"
  , "gap" := var tokens.space2
  ]

sheet :: Array Style
sheet = [ cardStyle, badgeStyle, separatorStyle, stackStyle, clusterStyle ]
