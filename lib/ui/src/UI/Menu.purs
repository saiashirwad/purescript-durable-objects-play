module UI.Menu
  ( Item
  , Input
  , component
  , sheet
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Button as Button
import UI.Core (bool, dataAttr, firstEnabled, lastEnabled, nextEnabled)
import UI.Internal.Dom as Dom
import UI.Style (Style, (:=), create, css, on, var)
import UI.Style as Style
import UI.Theme (tokens)
import Web.Event.Event (EventType(..), preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key, toEvent)

type Item output =
  { label :: String
  , disabled :: Boolean
  , value :: output
  }

type Input output =
  { id :: String
  , label :: String
  , items :: Array (Item output)
  , onOpenChange :: Boolean -> output
  }

type State output =
  { input :: Input output
  , open :: Boolean
  , active :: Int
  }

data Action output
  = Receive (Input output)
  | Open
  | Toggle
  | Focus Int
  | KeyDown KeyboardEvent
  | Choose Int

menuRef :: H.RefLabel
menuRef = H.RefLabel "menu"

itemRef :: Int -> H.RefLabel
itemRef index = H.RefLabel $ "menu-item-" <> show index

-- | A popover menu with one roving tab stop; the browser dismisses it.
component :: forall output query m. MonadAff m => H.Component query (Input output) output m
component = H.mkComponent
  { initialState: \input -> { input, open: false, active: firstEnabled input.items }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

render :: forall output m. State output -> H.ComponentHTML (Action output) () m
render state =
  HH.div [ css root ]
    [ Button.button Button.defaults
        [ HE.onClick \_ -> Open
        , ARIA.hasPopup "menu"
        , ARIA.expanded $ bool state.open
        , ARIA.controls state.input.id
        ]
        [ HH.text state.input.label ]
    , HH.div
        [ css popup
        , HP.id state.input.id
        , HP.ref menuRef
        , HP.attr (HH.AttrName "popover") "auto"
        , ARIA.role "menu"
        , ARIA.label state.input.label
        , HE.handler (EventType "toggle") \_ -> Toggle
        , dataAttr "ui" "menu"
        , dataAttr "open" $ bool state.open
        ]
        (Array.mapWithIndex (item state.active) state.input.items)
    ]

item :: forall output m. Int -> Int -> Item output -> H.ComponentHTML (Action output) () m
item active index entry =
  HH.button
    [ css menuItem
    , HP.type_ HP.ButtonButton
    , HP.disabled entry.disabled
    , HP.tabIndex if index == active then 0 else -1
    , HP.ref $ itemRef index
    , ARIA.role "menuitem"
    , ARIA.disabled $ bool entry.disabled
    , HE.onFocus \_ -> Focus index
    , HE.onKeyDown KeyDown
    , HE.onClick \_ -> Choose index
    , dataAttr "active" $ bool $ index == active
    ]
    [ HH.text entry.label ]

handleAction :: forall output m. MonadAff m => Action output -> H.HalogenM (State output) (Action output) () output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input, active = firstEnabled input.items }
  Open -> do
    H.modify_ \state -> state { open = true, active = firstEnabled state.input.items }
    Dom.onRef menuRef Dom.showPopover
    H.gets _.active >>= focus
  Toggle -> H.getHTMLElementRef menuRef >>= traverse_ \element -> do
    open <- liftEffect $ Dom.popoverOpen element
    state <- H.get
    when (open /= state.open) do
      H.modify_ _ { open = open }
      H.raise $ state.input.onOpenChange open
  Focus index -> H.modify_ _ { active = index }
  KeyDown event -> case key event of
    "ArrowDown" -> move event $ nextEnabled 1
    "ArrowUp" -> move event $ nextEnabled (-1)
    "Home" -> move event \items _ -> firstEnabled items
    "End" -> move event \items _ -> lastEnabled items
    "Escape" -> liftEffect (preventDefault $ toEvent event) *> close
    _ -> pure unit
  Choose index -> do
    items <- H.gets _.input.items
    case Array.index items index of
      Just chosen | not chosen.disabled -> H.raise chosen.value *> close
      _ -> pure unit
  where
  move event choose = do
    liftEffect $ preventDefault $ toEvent event
    state <- H.get
    let active = choose state.input.items state.active
    H.modify_ _ { active = active }
    focus active
  focus index = Dom.onRef (itemRef index) HTMLElement.focus
  close = Dom.onRef menuRef Dom.hidePopover

root :: Style
root = "display" := "inline-block"

popup :: Style
popup = create
  [ "background-color" := var tokens.surfaceRaised
  , "border" := "1px solid " <> var tokens.border
  , "border-radius" := var tokens.radiusMd
  , "box-shadow" := var tokens.shadow
  , "color" := var tokens.text
  , "inline-size" := "max-content"
  , "margin" := "0"
  , "min-inline-size" := "11rem"
  , "padding" := "0.3rem"
  ]

menuItem :: Style
menuItem = create
  [ "background" := "transparent"
  , "border" := "0"
  , "border-radius" := var tokens.radiusSm
  , "color" := "inherit"
  , "cursor" := "pointer"
  , "display" := "block"
  , "font" := "inherit"
  , "inline-size" := "100%"
  , "padding-block" := "0.5rem"
  , "padding-inline" := "0.7rem"
  , "text-align" := "start"
  , on Style.Hover [ "background-color" := var tokens.tint ]
  , on Style.FocusVisible
      [ "background-color" := var tokens.tint
      , "outline" := "2px solid " <> var tokens.focusRing
      , "outline-offset" := "-2px"
      ]
  , on Style.Selected [ "background-color" := var tokens.tint ]
  , on Style.Disabled
      [ "cursor" := "not-allowed"
      , "opacity" := "0.5"
      ]
  ]

sheet :: Array Style
sheet = [ root, popup, menuItem ]
