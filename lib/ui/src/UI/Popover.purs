module UI.Popover
  ( Input
  , component
  , sheet
  ) where

import Prelude

import Data.Bifunctor (bimap)
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
import UI.Core (Tone(..), bool, dataAttr)
import UI.Internal.Dom as Dom
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)
import Web.Event.Event (EventType(..))

type Input output =
  { id :: String
  , label :: String
  , trigger :: Array (HH.HTML Void output)
  , body :: Array (HH.HTML Void output)
  , onOpenChange :: Boolean -> output
  }

type State output =
  { input :: Input output
  , open :: Boolean
  }

data Action output
  = Receive (Input output)
  | Open
  | Close
  | Toggle
  | Raise output

popoverRef :: H.RefLabel
popoverRef = H.RefLabel "popover"

-- | A non-modal dialog on the popover API; light dismiss is the browser's.
component :: forall output query m. MonadAff m => H.Component query (Input output) output m
component = H.mkComponent
  { initialState: \input -> { input, open: false }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

render :: forall output m. State output -> H.ComponentHTML (Action output) () m
render { input, open } =
  HH.span [ css root ]
    [ Button.button Button.defaults
        [ HE.onClick \_ -> Open
        , ARIA.hasPopup "dialog"
        , ARIA.expanded $ bool open
        , ARIA.controls input.id
        ]
        (bimap absurd Raise <$> input.trigger)
    , HH.div
        [ css popup
        , HP.id input.id
        , HP.ref popoverRef
        , HP.attr (HH.AttrName "popover") "auto"
        , ARIA.role "dialog"
        , ARIA.label input.label
        , HE.handler (EventType "toggle") \_ -> Toggle
        , dataAttr "ui" "popover"
        , dataAttr "open" $ bool open
        ]
        [ HH.div [ css body ] $ bimap absurd Raise <$> input.body
        , HH.div [ css footer ]
            [ Button.button (Button.defaults { tone = Quiet }) [ HE.onClick \_ -> Close ] [ HH.text "Close" ] ]
        ]
    ]

handleAction :: forall output m. MonadAff m => Action output -> H.HalogenM (State output) (Action output) () output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  Open -> Dom.onRef popoverRef Dom.showPopover
  Close -> Dom.onRef popoverRef Dom.hidePopover
  Toggle -> H.getHTMLElementRef popoverRef >>= traverse_ \element -> do
    open <- liftEffect $ Dom.popoverOpen element
    state <- H.get
    when (open /= state.open) do
      H.modify_ _ { open = open }
      H.raise $ state.input.onOpenChange open
  Raise output -> H.raise output

root :: Style
root = "display" := "inline-block"

popup :: Style
popup = create
  [ "background-color" := var tokens.surfaceRaised
  , "border" := "1px solid " <> var tokens.border
  , "border-radius" := var tokens.radiusLg
  , "box-shadow" := var tokens.shadow
  , "color" := var tokens.text
  , "inline-size" := "min(24rem,calc(100% - 2rem))"
  , "margin" := "0"
  , "padding" := "0"
  ]

body :: Style
body = "padding" := "1.25rem"

footer :: Style
footer = create
  [ "border-block-start" := "1px solid " <> var tokens.border
  , "display" := "flex"
  , "justify-content" := "flex-end"
  , "padding" := "0.5rem"
  ]

sheet :: Array Style
sheet = [ root, popup, body, footer ]
