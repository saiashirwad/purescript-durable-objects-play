module UI.Theme
  ( Theme
  , ThemeName(..)
  , tokens
  , light
  , dark
  , highContrast
  , auto
  , scope
  , render
  ) where

import Prelude

import Data.String (joinWith)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import UI.Style (Style, Var, varName, variable)
import UI.Style as Style

data ThemeName = Light | Dark | HighContrast | Auto

derive instance Eq ThemeName

type Theme =
  { name :: ThemeName
  , values :: Array (Tuple Var String)
  }

-- | The semantic variables every module draws from. `tint` and `accentSoft`
-- | are translucent, so they read well on any surface.
tokens
  :: { background :: Var
     , surface :: Var
     , surfaceRaised :: Var
     , tint :: Var
     , text :: Var
     , textMuted :: Var
     , border :: Var
     , accent :: Var
     , accentHover :: Var
     , accentSoft :: Var
     , accentText :: Var
     , success :: Var
     , danger :: Var
     , dangerText :: Var
     , focusRing :: Var
     , shadow :: Var
     , radiusSm :: Var
     , radiusMd :: Var
     , radiusLg :: Var
     , space1 :: Var
     , space2 :: Var
     , space3 :: Var
     , space4 :: Var
     , fontBody :: Var
     , fontDisplay :: Var
     , fontMono :: Var
     }
tokens =
  { background: variable "color-background"
  , surface: variable "color-surface"
  , surfaceRaised: variable "color-surface-raised"
  , tint: variable "color-tint"
  , text: variable "color-text"
  , textMuted: variable "color-text-muted"
  , border: variable "color-border"
  , accent: variable "color-accent"
  , accentHover: variable "color-accent-hover"
  , accentSoft: variable "color-accent-soft"
  , accentText: variable "color-accent-text"
  , success: variable "color-success"
  , danger: variable "color-danger"
  , dangerText: variable "color-danger-text"
  , focusRing: variable "color-focus-ring"
  , shadow: variable "shadow-raised"
  , radiusSm: variable "radius-sm"
  , radiusMd: variable "radius-md"
  , radiusLg: variable "radius-lg"
  , space1: variable "space-1"
  , space2: variable "space-2"
  , space3: variable "space-3"
  , space4: variable "space-4"
  , fontBody: variable "font-body"
  , fontDisplay: variable "font-display"
  , fontMono: variable "font-mono"
  }

-- | Warm paper, ink text, an indigo accent.
light :: Theme
light =
  { name: Light
  , values:
      [ tokens.background /\ "oklch(97.5% 0.006 85)"
      , tokens.surface /\ "oklch(99.4% 0.002 85)"
      , tokens.surfaceRaised /\ "oklch(100% 0 0)"
      , tokens.tint /\ "oklch(22% 0.02 60 / 6%)"
      , tokens.text /\ "oklch(22% 0.02 60)"
      , tokens.textMuted /\ "oklch(52% 0.02 60)"
      , tokens.border /\ "oklch(22% 0.02 60 / 11%)"
      , tokens.accent /\ "oklch(49% 0.2 280)"
      , tokens.accentHover /\ "oklch(43% 0.21 280)"
      , tokens.accentSoft /\ "oklch(49% 0.2 280 / 12%)"
      , tokens.accentText /\ "oklch(99% 0 0)"
      , tokens.success /\ "oklch(64% 0.17 150)"
      , tokens.danger /\ "oklch(55% 0.2 25)"
      , tokens.dangerText /\ "oklch(99% 0 0)"
      , tokens.focusRing /\ "oklch(58% 0.2 280)"
      , tokens.shadow /\ "0 1px 2px oklch(22% 0.02 60 / 5%),0 10px 28px -14px oklch(22% 0.02 60 / 22%)"
      ] <> constants
  }

-- | Deep ink, the same accent lifted to read on it.
dark :: Theme
dark =
  { name: Dark
  , values:
      [ tokens.background /\ "oklch(14% 0.012 280)"
      , tokens.surface /\ "oklch(19% 0.014 280)"
      , tokens.surfaceRaised /\ "oklch(24% 0.016 280)"
      , tokens.tint /\ "oklch(100% 0 0 / 7%)"
      , tokens.text /\ "oklch(95% 0.008 85)"
      , tokens.textMuted /\ "oklch(70% 0.02 280)"
      , tokens.border /\ "oklch(100% 0 0 / 10%)"
      , tokens.accent /\ "oklch(76% 0.13 280)"
      , tokens.accentHover /\ "oklch(82% 0.12 280)"
      , tokens.accentSoft /\ "oklch(76% 0.13 280 / 16%)"
      , tokens.accentText /\ "oklch(18% 0.06 280)"
      , tokens.success /\ "oklch(75% 0.16 150)"
      , tokens.danger /\ "oklch(72% 0.17 25)"
      , tokens.dangerText /\ "oklch(18% 0.05 25)"
      , tokens.focusRing /\ "oklch(80% 0.12 280)"
      , tokens.shadow /\ "0 1px 2px oklch(0% 0 0 / 40%),0 14px 36px -14px oklch(0% 0 0 / 65%)"
      ] <> constants
  }

-- | System colors only; the browser owns contrast.
highContrast :: Theme
highContrast =
  { name: HighContrast
  , values:
      [ tokens.background /\ "Canvas"
      , tokens.surface /\ "Canvas"
      , tokens.surfaceRaised /\ "Canvas"
      , tokens.tint /\ "Canvas"
      , tokens.text /\ "CanvasText"
      , tokens.textMuted /\ "CanvasText"
      , tokens.border /\ "CanvasText"
      , tokens.accent /\ "Highlight"
      , tokens.accentHover /\ "Highlight"
      , tokens.accentSoft /\ "Canvas"
      , tokens.accentText /\ "HighlightText"
      , tokens.success /\ "Highlight"
      , tokens.danger /\ "Mark"
      , tokens.dangerText /\ "MarkText"
      , tokens.focusRing /\ "Highlight"
      , tokens.shadow /\ "none"
      ] <> constants
  }

-- | Light by default; dark or high contrast when the system asks.
auto :: Theme
auto = light { name = Auto }

constants :: Array (Tuple Var String)
constants =
  [ tokens.radiusSm /\ "0.5rem"
  , tokens.radiusMd /\ "0.875rem"
  , tokens.radiusLg /\ "1.25rem"
  , tokens.space1 /\ "0.25rem"
  , tokens.space2 /\ "0.5rem"
  , tokens.space3 /\ "0.75rem"
  , tokens.space4 /\ "1rem"
  , tokens.fontBody /\ "'Instrument Sans',system-ui,-apple-system,sans-serif"
  , tokens.fontDisplay /\ "'Instrument Serif',Georgia,'Times New Roman',serif"
  , tokens.fontMono /\ "'JetBrains Mono',ui-monospace,SFMono-Regular,Menlo,monospace"
  ]

-- | Put a theme on an ancestor; everything inside reads its variables.
scope :: forall r i. Theme -> Style -> HH.IProp (class :: String | r) i
scope theme style = HP.classes $ [ HH.ClassName $ themeClass theme.name ] <> Style.classes style

render :: String
render = joinWith (CodeUnits.singleton '\n')
  [ themeRule light
  , themeRule dark
  , themeRule highContrast
  , themeRule auto
  , "@media (prefers-color-scheme:dark){." <> themeClass Auto <> "{" <> declarations dark.values <> "}}"
  , "@media (forced-colors:active){." <> themeClass Auto <> "{" <> declarations highContrast.values <> "}}"
  ]

themeRule :: Theme -> String
themeRule theme = "." <> themeClass theme.name <> "{" <> declarations theme.values
  <> ";font-family:"
  <> Style.var tokens.fontBody
  <> ";color-scheme:"
  <> colorScheme theme.name
  <> "}"

colorScheme :: ThemeName -> String
colorScheme = case _ of
  Light -> "light"
  Dark -> "dark"
  HighContrast -> "light dark"
  Auto -> "light dark"

declarations :: Array (Tuple Var String) -> String
declarations values = joinWith ";" $ values <#> \(Tuple token value) -> varName token <> ":" <> value

themeClass :: ThemeName -> String
themeClass = case _ of
  Light -> "ui-theme-light"
  Dark -> "ui-theme-dark"
  HighContrast -> "ui-theme-high-contrast"
  Auto -> "ui-theme-auto"
