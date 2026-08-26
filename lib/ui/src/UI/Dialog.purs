module UI.Dialog
  ( Kind(..)
  , Input
  , component
  , sheet
  , raw
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Foldable (foldMap)
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
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Kind = Modal | Alert

derive instance Eq Kind

type Input output =
  { id :: String
  , kind :: Kind
  , title :: String
  , description :: Maybe String
  , trigger :: Array (HH.HTML Void output)
  , body :: Array (HH.HTML Void output)
  , closeLabel :: String
  , initialOpen :: Boolean
  , onOpenChange :: Boolean -> output
  }

type State output =
  { input :: Input output
  , open :: Boolean
  }

data Action output
  = Initialize
  | Receive (Input output)
  | Open
  | Close
  | NativeClosed
  | Backdrop MouseEvent
  | Raise output

dialogRef :: H.RefLabel
dialogRef = H.RefLabel "dialog"

-- | A native `<dialog>`: the browser contains focus and handles Escape.
component :: forall output query m. MonadAff m => H.Component query (Input output) output m
component = H.mkComponent
  { initialState: \input -> { input, open: input.initialOpen }
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

render :: forall output m. State output -> H.ComponentHTML (Action output) () m
render { input, open } =
  HH.div_
    [ Button.button Button.defaults [ HE.onClick \_ -> Open ] $ bimap absurd Raise <$> input.trigger
    , HH.dialog
        [ css dialog
        , HP.id input.id
        , HP.ref dialogRef
        , ARIA.role case input.kind of
            Modal -> "dialog"
            Alert -> "alertdialog"
        , ARIA.labelledBy titleId
        , ARIA.describedBy $ foldMap (const descriptionId) input.description
        , HE.handler (EventType "close") \_ -> NativeClosed
        , HE.onClick Backdrop
        , dataAttr "ui" "dialog"
        , dataAttr "open" $ bool open
        ]
        [ HH.div [ css panel ]
            [ HH.header [ css header ]
                [ HH.div_ $
                    [ HH.h2 [ css title, HP.id titleId ] [ HH.text input.title ] ]
                      <> foldMap (\text -> [ HH.p [ css description, HP.id descriptionId ] [ HH.text text ] ]) input.description
                , Button.iconButton input.closeLabel (Button.defaults { tone = Quiet }) [ HE.onClick \_ -> Close ] [ HH.text "×" ]
                ]
            , HH.div [ css body ] $ bimap absurd Raise <$> input.body
            ]
        ]
    ]
  where
  titleId = input.id <> "-title"
  descriptionId = input.id <> "-description"

handleAction :: forall output m. MonadAff m => Action output -> H.HalogenM (State output) (Action output) () output m Unit
handleAction = case _ of
  Initialize -> H.gets _.open >>= \open -> when open openDialog
  Receive input -> H.modify_ _ { input = input }
  Open -> openDialog
  Close -> close
  NativeClosed -> H.gets _.open >>= \open -> when open do
    Dom.onRef dialogRef Dom.closeDialog
    announce false
  Backdrop event -> when (Dom.isBackdropClick $ toEvent event) close
  Raise output -> H.raise output
  where
  openDialog = do
    Dom.onRef dialogRef Dom.showModal
    announce true
  close = H.getHTMLElementRef dialogRef >>= case _ of
    Just element -> liftEffect $ Dom.closeDialog element
    Nothing -> announce false
  announce open = do
    H.modify_ _ { open = open }
    onOpenChange <- H.gets _.input.onOpenChange
    H.raise $ onOpenChange open

dialog :: Style
dialog = create
  [ "background" := "transparent"
  , "border" := "0"
  , "color" := var tokens.text
  , "inline-size" := "min(36rem,calc(100% - 2rem))"
  , "max-block-size" := "calc(100dvh - 2rem)"
  , "padding" := "0"
  ]

panel :: Style
panel = create
  [ "background-color" := var tokens.surfaceRaised
  , "border" := "1px solid " <> var tokens.border
  , "border-radius" := var tokens.radiusLg
  , "box-shadow" := var tokens.shadow
  , "display" := "grid"
  , "max-block-size" := "calc(100dvh - 2rem)"
  , "overflow" := "auto"
  ]

header :: Style
header = create
  [ "align-items" := "flex-start"
  , "display" := "flex"
  , "gap" := "1rem"
  , "justify-content" := "space-between"
  , "padding" := "1.5rem 1.5rem 1rem"
  ]

title :: Style
title = create
  [ "font-family" := var tokens.fontDisplay
  , "font-size" := "1.625rem"
  , "font-weight" := "400"
  , "letter-spacing" := "-0.01em"
  , "line-height" := "1.2"
  , "margin" := "0"
  ]

description :: Style
description = create
  [ "color" := var tokens.textMuted
  , "font-size" := "0.9375rem"
  , "margin-block" := "0.35rem 0"
  ]

body :: Style
body = create
  [ "padding-block" := "0 1.5rem"
  , "padding-inline" := "1.5rem"
  ]

sheet :: Array Style
sheet = [ dialog, panel, header, title, description, body ]

raw :: String
raw = "[data-ui=dialog]::backdrop{background:oklch(10% 0.02 280 / 55%);backdrop-filter:blur(4px)}"
