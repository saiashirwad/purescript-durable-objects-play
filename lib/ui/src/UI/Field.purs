module UI.Field
  ( Options
  , defaults
  , input
  , password
  , textarea
  , field
  , describedBy
  , sheet
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import UI.Core (bool, dataAttr)
import UI.Input as Input
import UI.Style (Style, (:=), create, css, var)
import UI.Theme (tokens)

type Options =
  { id :: String
  , label :: String
  , description :: Maybe String
  , error :: Maybe String
  , required :: Boolean
  , disabled :: Boolean
  , styles :: Style
  }

defaults :: String -> String -> Options
defaults id label =
  { id
  , label
  , description: Nothing
  , error: Nothing
  , required: false
  , disabled: false
  , styles: mempty
  }

input :: forall w i. Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
input = inputWith Input.text

password :: forall w i. Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i
password = inputWith Input.password

inputWith
  :: forall w i
   . (Input.Options -> Array (HH.IProp I.HTMLinput i) -> HH.HTML w i)
  -> Options
  -> Array (HH.IProp I.HTMLinput i)
  -> HH.HTML w i
inputWith render options properties = field options $ render
  (Input.defaults { disabled = options.disabled, invalid = isJust options.error, styles = options.styles })
  ([ HP.id options.id, HP.required options.required, describedBy options ] <> properties)

textarea :: forall w i. Options -> Array (HH.IProp I.HTMLtextarea i) -> HH.HTML w i
textarea options properties = field options $ HH.textarea $
  [ css $ Input.control <> tall <> options.styles
  , HP.id options.id
  , HP.required options.required
  , HP.disabled options.disabled
  , ARIA.invalid $ bool $ isJust options.error
  , describedBy options
  ] <> properties

-- | A label, the control, then its description and error, all connected.
field :: forall w i. Options -> HH.HTML w i -> HH.HTML w i
field options control = HH.div [ css root, dataAttr "ui" "field" ] $ fold
  [ [ HH.label [ css labelStyle, HP.for options.id ] $
        [ HH.text options.label ] <> guard options.required [ HH.span [ ARIA.hidden "true" ] [ HH.text " *" ] ]
    ]
  , [ control ]
  , foldMap (\text -> [ HH.p [ css help, HP.id $ helpId options ] [ HH.text text ] ]) options.description
  , foldMap (\text -> [ HH.p [ css errorStyle, HP.id $ errorId options, ARIA.role "alert" ] [ HH.text text ] ]) options.error
  ]

describedBy :: forall r i. Options -> HH.IProp r i
describedBy options = ARIA.describedBy $ joinWith " " $ Array.catMaybes
  [ helpId options <$ options.description
  , errorId options <$ options.error
  ]

helpId :: Options -> String
helpId options = options.id <> "-help"

errorId :: Options -> String
errorId options = options.id <> "-error"

root :: Style
root = create
  [ "display" := "grid"
  , "gap" := var tokens.space2
  , "inline-size" := "100%"
  , "text-align" := "start"
  ]

labelStyle :: Style
labelStyle = create
  [ "color" := var tokens.text
  , "font-size" := "0.875rem"
  , "font-weight" := "500"
  ]

tall :: Style
tall = create
  [ "min-block-size" := "7rem"
  , "resize" := "vertical"
  ]

help :: Style
help = create
  [ "color" := var tokens.textMuted
  , "font-size" := "0.8125rem"
  , "margin" := "0"
  ]

errorStyle :: Style
errorStyle = create
  [ "color" := var tokens.danger
  , "font-size" := "0.8125rem"
  , "margin" := "0"
  ]

sheet :: Array Style
sheet = [ root, labelStyle, tall, help, errorStyle ]
