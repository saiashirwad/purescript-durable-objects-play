module UI.Lab
  ( main
  , sheet
  ) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import UI.Button as Button
import UI.Checkbox as Checkbox
import UI.Core (Size(..), Tone(..))
import UI.Dialog as Dialog
import UI.Disclosure as Disclosure
import UI.Feedback as Feedback
import UI.Field as Field
import UI.Menu as Menu
import UI.Popover as Popover
import UI.RadioGroup as RadioGroup
import UI.Select as Select
import UI.Slider as Slider
import UI.Status as Status
import UI.Style (Style, (:=), create, css, var)
import UI.Surface as Surface
import UI.Tabs as Tabs
import UI.Theme (ThemeName(..), tokens)
import UI.Theme as Theme
import UI.Tooltip as Tooltip

-- | Every module on one page, under a theme you can switch.

type State =
  { theme :: ThemeName
  , name :: String
  , checked :: Boolean
  , choice :: String
  , notice :: String
  }

data Action
  = SetTheme ThemeName
  | SetName String
  | SetChecked Boolean
  | SetChoice String
  | Notice String
  | OpenChanged String Boolean
  | TabChanged Int

type Slots =
  ( dialog :: H.Slot (Const Void) Action Unit
  , menu :: H.Slot (Const Void) Action Unit
  , popover :: H.Slot (Const Void) Action Unit
  , tabs :: H.Slot (Const Void) Action Unit
  )

type Html m = H.ComponentHTML Action Slots m

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI page unit body

page :: forall query input output m. MonadAff m => H.Component query input output m
page = H.mkComponent
  { initialState: \_ -> { theme: Auto, name: "", checked: false, choice: "daily", notice: "Ready" }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  }

render :: forall m. MonadAff m => State -> Html m
render state =
  HH.main [ Theme.scope (theme state.theme) pageStyle ]
    [ HH.header [ css header ]
        [ HH.div_
            [ HH.h1 [ css title ] [ HH.text "durable-ui lab" ]
            , HH.p [ css intro ] [ HH.text "Accessible Halogen modules with scoped themes and deterministic styles." ]
            ]
        , Surface.cluster
            [ themeButton state Light "Light"
            , themeButton state Dark "Dark"
            , themeButton state HighContrast "High contrast"
            , themeButton state Auto "Automatic"
            ]
        ]
    , HH.div [ css grid ]
        [ sample "Buttons" [ buttons ]
        , sample "Fields" (fields state)
        , sample "Choice controls" (choices state)
        , sample "Layers" [ layers ]
        , sample "Tabs" [ tabs ]
        , sample "Disclosure and feedback" feedback
        ]
    , Status.polite [ HH.text state.notice ]
    ]

buttons :: forall m. Html m
buttons = Surface.cluster
  [ Button.button Button.defaults [] [ HH.text "Neutral" ]
  , Button.button (Button.defaults { tone = Accent }) [] [ HH.text "Accent" ]
  , Button.button (Button.defaults { tone = Danger }) [] [ HH.text "Danger" ]
  , Button.button (Button.defaults { tone = Quiet }) [] [ HH.text "Quiet" ]
  , Button.button (Button.defaults { disabled = true }) [] [ HH.text "Disabled" ]
  , Button.button (Button.defaults { busy = true }) [] [ HH.text "Busy" ]
  , Tooltip.iconButton { id: "lab-help", label: "Help", button: Button.defaults } [] [ HH.text "?" ]
  ]

fields :: forall m. State -> Array (Html m)
fields state =
  [ Field.input
      ((Field.defaults "lab-name" "Name") { description = Just "This label and description are connected to the input." })
      [ HP.placeholder "Ada Lovelace", HP.value state.name, HE.onValueInput SetName ]
  , Field.password
      ((Field.defaults "lab-password" "Password") { error = error, required = true })
      []
  , Select.select
      ((Field.defaults "lab-role" "Role") { description = Just "Native selection behavior is the default." })
      [ { value: "author", label: "Author", disabled: false }
      , { value: "editor", label: "Editor", disabled: false }
      , { value: "owner", label: "Owner", disabled: true }
      ]
      []
  ]
  where
  error = if state.name == "error" then Just "Use a different value." else Nothing

choices :: forall m. State -> Array (Html m)
choices state =
  [ Surface.cluster
      [ Checkbox.checkbox { id: "lab-checkbox", label: "Email updates", checked: state.checked, disabled: false } [ HE.onChecked SetChecked ]
      , Checkbox.switch { id: "lab-switch", label: "Compact mode", checked: state.checked, disabled: false } [ HE.onChecked SetChecked ]
      ]
  , RadioGroup.radioGroup
      { id: "lab-frequency"
      , name: "frequency"
      , label: "Summary frequency"
      , description: Just "Arrow keys use native radio-group behavior."
      , value: state.choice
      , onChange: SetChoice
      }
      [ { value: "daily", label: "Daily", disabled: false }
      , { value: "weekly", label: "Weekly", disabled: false }
      , { value: "never", label: "Never", disabled: false }
      ]
  , Slider.slider
      { id: "lab-volume"
      , label: "Volume"
      , value: 40.0
      , minimum: 0.0
      , maximum: 100.0
      , step: 10.0
      , disabled: false
      , onChange: \value -> Notice $ "Volume " <> value
      }
  ]

layers :: forall m. MonadAff m => Html m
layers = Surface.cluster
  [ HH.slot (Proxy :: Proxy "dialog") unit Dialog.component
      { id: "lab-dialog"
      , kind: Dialog.Modal
      , title: "Confirm the change"
      , description: Just "The native modal dialog owns focus containment and Escape behavior."
      , trigger: [ HH.text "Open dialog" ]
      , body:
          [ Surface.stack
              [ HH.p_ [ HH.text "Interactive content can emit actions to the parent." ]
              , Button.button (Button.defaults { tone = Accent }) [ HE.onClick \_ -> Notice "Dialog action selected" ] [ HH.text "Run action" ]
              ]
          ]
      , closeLabel: "Close dialog"
      , initialOpen: false
      , onOpenChange: OpenChanged "Dialog"
      }
      identity
  , HH.slot (Proxy :: Proxy "popover") unit Popover.component
      { id: "lab-popover"
      , label: "Popover example"
      , trigger: [ HH.text "Open popover" ]
      , body: [ HH.p_ [ HH.text "The browser supplies light dismissal and Escape behavior." ] ]
      , onOpenChange: OpenChanged "Popover"
      }
      identity
  , HH.slot (Proxy :: Proxy "menu") unit Menu.component
      { id: "lab-menu"
      , label: "Actions"
      , items:
          [ { label: "Duplicate", disabled: false, value: Notice "Duplicate selected" }
          , { label: "Archive", disabled: false, value: Notice "Archive selected" }
          , { label: "Unavailable", disabled: true, value: Notice "Unavailable selected" }
          ]
      , onOpenChange: OpenChanged "Menu"
      }
      identity
  ]

tabs :: forall m. MonadAff m => Html m
tabs = HH.slot (Proxy :: Proxy "tabs") unit Tabs.component
  { id: "lab-tabs"
  , label: "Account settings"
  , orientation: Tabs.Horizontal
  , initial: 0
  , items:
      [ { id: "profile", label: "Profile", disabled: false, content: [ HH.p_ [ HH.text "Profile settings" ] ] }
      , { id: "security", label: "Security", disabled: false, content: [ HH.p_ [ HH.text "Security settings" ] ] }
      , { id: "billing", label: "Billing", disabled: true, content: [ HH.p_ [ HH.text "Billing settings" ] ] }
      ]
  , onChange: TabChanged
  }
  identity

feedback :: forall m. Array (Html m)
feedback =
  [ Disclosure.accordion "lab-faq"
      [ { id: "lab-faq-one", title: "Can themes be nested?", open: true, content: [ HH.p_ [ HH.text "Yes. A theme scope can start at any ancestor." ] ] }
      , { id: "lab-faq-two", title: "Does motion respect user settings?", open: false, content: [ HH.p_ [ HH.text "Yes. Reduced motion is part of the generated base CSS." ] ] }
      ]
  , Feedback.progress { label: "Upload", value: 64.0, maximum: 100.0 }
  , Surface.cluster [ Feedback.spinner "Loading", HH.text "Loading" ]
  , Feedback.toastRegion "Notifications" [ { id: "lab-toast", title: "Saved", description: "Your changes are ready." } ]
  ]

sample :: forall m. String -> Array (Html m) -> Html m
sample heading content = Surface.card
  [ HH.h2 [ css sectionTitle ] [ HH.text heading ]
  , Surface.stack content
  ]

themeButton :: forall m. State -> ThemeName -> String -> Html m
themeButton state name label = Button.button
  (Button.defaults { size = Small, tone = if state.theme == name then Accent else Neutral })
  [ HE.onClick \_ -> SetTheme name ]
  [ HH.text label ]

handleAction :: forall output m. Action -> H.HalogenM State Action Slots output m Unit
handleAction = case _ of
  SetTheme name -> H.modify_ _ { theme = name, notice = "Theme changed" }
  SetName name -> H.modify_ _ { name = name }
  SetChecked checked -> H.modify_ _ { checked = checked, notice = if checked then "Enabled" else "Disabled" }
  SetChoice choice -> H.modify_ _ { choice = choice, notice = "Frequency changed to " <> choice }
  Notice notice -> H.modify_ _ { notice = notice }
  OpenChanged name open -> H.modify_ _ { notice = name <> if open then " opened" else " closed" }
  TabChanged index -> H.modify_ _ { notice = "Tab " <> show (index + 1) <> " selected" }

theme :: ThemeName -> Theme.Theme
theme = case _ of
  Light -> Theme.light
  Dark -> Theme.dark
  HighContrast -> Theme.highContrast
  Auto -> Theme.auto

pageStyle :: Style
pageStyle = create
  [ "background-color" := var tokens.background
  , "color" := var tokens.text
  , "min-block-size" := "100dvh"
  , "padding" := "clamp(1rem,4vw,3rem)"
  ]

header :: Style
header = create
  [ "align-items" := "flex-start"
  , "display" := "flex"
  , "flex-wrap" := "wrap"
  , "gap" := "1rem"
  , "justify-content" := "space-between"
  , "margin-block-end" := "2rem"
  ]

title :: Style
title = create
  [ "font-family" := var tokens.fontDisplay
  , "font-size" := "clamp(2.25rem,6vw,3.5rem)"
  , "font-weight" := "400"
  , "letter-spacing" := "-0.02em"
  , "line-height" := "1"
  , "margin" := "0"
  ]

intro :: Style
intro = create
  [ "color" := var tokens.textMuted
  , "margin" := "0.5rem 0 0"
  ]

grid :: Style
grid = create
  [ "display" := "grid"
  , "gap" := "1rem"
  , "grid-template-columns" := "repeat(auto-fit,minmax(min(100%,24rem),1fr))"
  ]

sectionTitle :: Style
sectionTitle = create
  [ "font-size" := "0.8125rem"
  , "font-weight" := "500"
  , "letter-spacing" := "0.06em"
  , "margin" := "0 0 1rem"
  , "text-transform" := "uppercase"
  , "color" := var tokens.textMuted
  ]

sheet :: Array Style
sheet = [ pageStyle, header, title, intro, grid, sectionTitle ]
