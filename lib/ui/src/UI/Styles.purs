module UI.Styles
  ( css
  ) where

import Prelude

import Data.Foldable (fold)
import Data.String (joinWith)
import Data.String.CodeUnits (singleton)
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Checkbox as Checkbox
import UI.Dialog as Dialog
import UI.Disclosure as Disclosure
import UI.Feedback as Feedback
import UI.Field as Field
import UI.Icon as Icon
import UI.Input as Input
import UI.Lab as Lab
import UI.Menu as Menu
import UI.Popover as Popover
import UI.RadioGroup as RadioGroup
import UI.Select as Select
import UI.Slider as Slider
import UI.Status as Status
import UI.Style (var)
import UI.Style as Style
import UI.Surface as Surface
import UI.Tabs as Tabs
import UI.Theme (tokens)
import UI.Theme as Theme
import UI.Tooltip as Tooltip

-- | The one stylesheet: a reset, the themes, every module's atoms, then the
-- | few rules atoms cannot express.
css :: String
css = joinWith (singleton '\n')
  [ reset
  , Theme.render
  , Style.renderSheet components
  ]

components :: Style.Sheet
components =
  Style.atoms
    ( fold
        [ Avatar.sheet
        , Button.sheet
        , Checkbox.sheet
        , Dialog.sheet
        , Disclosure.sheet
        , Feedback.sheet
        , Field.sheet
        , Icon.sheet
        , Input.sheet
        , Lab.sheet
        , Menu.sheet
        , Popover.sheet
        , RadioGroup.sheet
        , Select.sheet
        , Slider.sheet
        , Status.sheet
        , Surface.sheet
        , Tabs.sheet
        , Tooltip.sheet
        ]
    )
    <> fold
      ( Style.global <$>
          [ Checkbox.raw
          , Dialog.raw
          , Feedback.raw
          , Tooltip.raw
          ]
      )

reset :: String
reset = joinWith ""
  [ "*,*::before,*::after{box-sizing:border-box}"
  , "html{line-height:1.5;-webkit-font-smoothing:antialiased;-moz-osx-font-smoothing:grayscale;text-rendering:optimizeLegibility}"
  , "body{background:" <> var tokens.background <> ";color:" <> var tokens.text <> ";font-family:" <> var tokens.fontBody <> ";margin:0}"
  , "button,input,select,textarea{font:inherit}"
  , "button:focus-visible,input:focus-visible,select:focus-visible,textarea:focus-visible{scroll-margin:1rem}"
  , "@media (prefers-reduced-motion:reduce){*,*::before,*::after{scroll-behavior:auto!important;transition-duration:.01ms!important;animation-duration:.01ms!important;animation-iteration-count:1!important}}"
  ]
