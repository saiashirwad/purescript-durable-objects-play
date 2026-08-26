module UI.Disclosure
  ( Item
  , accordion
  , collapsible
  , sheet
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Core (dataAttr)
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)

type Item w i =
  { id :: String
  , title :: String
  , open :: Boolean
  , content :: Array (HH.HTML w i)
  }

-- | Details elements that share a `name`, so the browser keeps one open.
accordion :: forall w i. String -> Array (Item w i) -> HH.HTML w i
accordion name items = HH.div [ css root, dataAttr "ui" "accordion" ] $ item (Just name) <$> items

collapsible :: forall w i. Item w i -> HH.HTML w i
collapsible = item Nothing

item :: forall w i. Maybe String -> Item w i -> HH.HTML w i
item group entry = HH.details
  ( [ css details, HP.id entry.id ]
      <> guard entry.open [ HP.attr (HH.AttrName "open") "" ]
      <> foldMap (\name -> [ HP.attr (HH.AttrName "name") name ]) group
  )
  [ HH.summary [ css summary ] [ HH.text entry.title ]
  , HH.div [ css content ] entry.content
  ]

root :: Style
root = "border-block-start" := "1px solid " <> var tokens.border

details :: Style
details = "border-block-end" := "1px solid " <> var tokens.border

summary :: Style
summary = create
  [ "cursor" := "pointer"
  , "font-weight" := "500"
  , "padding-block" := "0.85rem"
  , "padding-inline" := "0.25rem"
  , on Style.FocusVisible
      [ "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "2px"
      ]
  ]

content :: Style
content = create
  [ "color" := var tokens.textMuted
  , "padding-block" := "0 1rem"
  , "padding-inline" := "0.25rem"
  ]

sheet :: Array Style
sheet = [ root, details, summary, content ]
