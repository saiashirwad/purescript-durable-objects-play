module UI.Tabs
  ( Orientation(..)
  , Item
  , Input
  , component
  , sheet
  ) where

import Prelude

import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Core (bool, dataAttr, firstEnabled, lastEnabled, nextEnabled)
import UI.Internal.Dom as Dom
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, toEvent)

data Orientation = Horizontal | Vertical

derive instance Eq Orientation

type Item output =
  { id :: String
  , label :: String
  , disabled :: Boolean
  , content :: Array (HH.HTML Void output)
  }

type Input output =
  { id :: String
  , label :: String
  , orientation :: Orientation
  , initial :: Int
  , items :: Array (Item output)
  , onChange :: Int -> output
  }

type State output =
  { input :: Input output
  , active :: Int
  }

data Action output
  = Receive (Input output)
  | Select Int
  | KeyDown KeyboardEvent
  | Raise output

tabRef :: Int -> H.RefLabel
tabRef index = H.RefLabel $ "tab-" <> show index

-- | A tab list with one roving tab stop; arrows follow the orientation.
component :: forall output query m. MonadAff m => H.Component query (Input output) output m
component = H.mkComponent
  { initialState: \input -> { input, active: validInitial input }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

render :: forall output m. State output -> H.ComponentHTML (Action output) () m
render { input, active } =
  HH.div [ css root, dataAttr "ui" "tabs" ]
    [ HH.div
        [ css $ list <> guard (input.orientation == Vertical) vertical
        , HP.id input.id
        , ARIA.role "tablist"
        , ARIA.label input.label
        , ARIA.orientation case input.orientation of
            Horizontal -> "horizontal"
            Vertical -> "vertical"
        ]
        (Array.mapWithIndex tab input.items)
    , HH.div_ $ Array.mapWithIndex panel input.items
    ]
  where
  tabId item = input.id <> "-tab-" <> item.id
  panelId item = input.id <> "-panel-" <> item.id

  tab index item = HH.button
    [ css tabStyle
    , HP.type_ HP.ButtonButton
    , HP.id $ tabId item
    , HP.disabled item.disabled
    , HP.tabIndex if index == active then 0 else -1
    , HP.ref $ tabRef index
    , ARIA.role "tab"
    , ARIA.controls $ panelId item
    , ARIA.selected $ bool $ index == active
    , HE.onClick \_ -> Select index
    , HE.onKeyDown KeyDown
    , dataAttr "active" $ bool $ index == active
    ]
    [ HH.text item.label ]

  panel index item = HH.div
    ( [ css panelStyle
      , HP.id $ panelId item
      , HP.tabIndex 0
      , ARIA.role "tabpanel"
      , ARIA.labelledBy $ tabId item
      ] <> guard (index /= active) [ HP.attr (HH.AttrName "hidden") "" ]
    )
    (bimap absurd Raise <$> item.content)

handleAction :: forall output m. MonadAff m => Action output -> H.HalogenM (State output) (Action output) () output m Unit
handleAction = case _ of
  Receive input -> H.modify_ \state -> state { input = input, active = clampActive input state.active }
  Select index -> select index
  KeyDown event -> do
    { input, active } <- H.get
    let
      next = case key event, input.orientation of
        "ArrowRight", Horizontal -> Just $ nextEnabled 1 input.items active
        "ArrowLeft", Horizontal -> Just $ nextEnabled (-1) input.items active
        "ArrowDown", Vertical -> Just $ nextEnabled 1 input.items active
        "ArrowUp", Vertical -> Just $ nextEnabled (-1) input.items active
        "Home", _ -> Just $ firstEnabled input.items
        "End", _ -> Just $ lastEnabled input.items
        _, _ -> Nothing
    for_ next \index -> do
      liftEffect $ preventDefault $ toEvent event
      select index
      Dom.onRef (tabRef index) focus
  Raise output -> H.raise output
  where
  select index = do
    { input, active } <- H.get
    case Array.index input.items index of
      Just item | not item.disabled, index /= active -> do
        H.modify_ _ { active = index }
        H.raise $ input.onChange index
      _ -> pure unit

validInitial :: forall output. Input output -> Int
validInitial input = case Array.index input.items input.initial of
  Just item | not item.disabled -> input.initial
  _ -> firstEnabled input.items

clampActive :: forall output. Input output -> Int -> Int
clampActive input active = case Array.index input.items active of
  Just item | not item.disabled -> active
  _ -> validInitial input

root :: Style
root = "display" := "grid"

list :: Style
list = create
  [ "background-color" := var tokens.tint
  , "border-radius" := var tokens.radiusMd
  , "display" := "flex"
  , "gap" := "0.2rem"
  , "padding" := "0.25rem"
  ]

vertical :: Style
vertical = "flex-direction" := "column"

tabStyle :: Style
tabStyle = create
  [ "background" := "transparent"
  , "border" := "0"
  , "border-radius" := var tokens.radiusSm
  , "color" := var tokens.textMuted
  , "cursor" := "pointer"
  , "font" := "inherit"
  , "font-weight" := "500"
  , "min-block-size" := "2.25rem"
  , "padding-inline" := "0.9rem"
  , "transition" := "background-color 120ms ease,color 120ms ease,box-shadow 120ms ease"
  , on Style.Hover [ "color" := var tokens.text ]
  , on Style.Selected
      [ "background-color" := var tokens.surfaceRaised
      , "box-shadow" := var tokens.shadow
      , "color" := var tokens.text
      ]
  , on Style.FocusVisible
      [ "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "1px"
      ]
  , on Style.Disabled
      [ "cursor" := "not-allowed"
      , "opacity" := "0.45"
      ]
  ]

panelStyle :: Style
panelStyle = create
  [ "padding-block" := "1rem"
  , on Style.FocusVisible
      [ "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "2px"
      ]
  ]

sheet :: Array Style
sheet = [ root, list, vertical, tabStyle, panelStyle ]
