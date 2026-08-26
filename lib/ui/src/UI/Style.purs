module UI.Style
  ( Style
  , State(..)
  , Preference(..)
  , Var
  , rule
  , (:=)
  , create
  , on
  , prefer
  , variable
  , var
  , varName
  , css
  , classes
  , inlineVars
  , render
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (fold)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | An element state a declaration can wait for.
data State
  = Hover
  | Active
  | FocusVisible
  | Disabled
  | Checked
  | Open
  | Selected
  | Invalid

derive instance Eq State

-- | A user preference a declaration can wait for.
data Preference
  = Dark
  | ReducedMotion
  | MoreContrast
  | ForcedColors

derive instance Eq Preference

data Condition
  = Base
  | InState State
  | Under Preference

derive instance Eq Condition

type Declaration =
  { condition :: Condition
  , property :: String
  , value :: String
  }

-- | Declarations in order. For one property under one condition, the last wins,
-- | so `recipe <> caller` lets the caller override.
newtype Style = Style (Array Declaration)

derive newtype instance Semigroup Style
derive newtype instance Monoid Style

newtype Var = Var String

foreign import atomName :: String -> String

-- | One declaration. Use longhand or logical properties so composition has
-- | one clear result.
rule :: String -> String -> Style
rule property value = Style [ { condition: Base, property, value } ]

infix 4 rule as :=

create :: Array Style -> Style
create = fold

-- | These declarations apply only while the element is in `state`.
on :: State -> Array Style -> Style
on = under <<< InState

-- | These declarations apply only under a user `preference`.
prefer :: Preference -> Array Style -> Style
prefer = under <<< Under

under :: Condition -> Array Style -> Style
under condition styles = Style $ declarations <#> _ { condition = condition }
  where
  Style declarations = fold styles

-- | A custom property; set it with `inlineVars`, read it with `var`.
variable :: String -> Var
variable name = Var ("--ui-" <> name)

var :: Var -> String
var (Var name) = "var(" <> name <> ")"

varName :: Var -> String
varName (Var name) = name

-- | The `class` attribute for a style.
css :: forall r i. Style -> HH.IProp (class :: String | r) i
css = HP.classes <<< classes

classes :: Style -> Array HH.ClassName
classes (Style declarations) = HH.ClassName <<< className <$> resolve declarations

inlineVars :: forall r i. Array (Tuple Var String) -> HH.IProp (style :: String | r) i
inlineVars values = HP.style $ joinWith ";" $ values <#> \(Tuple (Var name) value) -> name <> ":" <> value

-- | The stylesheet: one class per distinct declaration that can still win.
render :: Array Style -> String
render styles = joinWith "\n" $ renderDeclaration <$> Array.nubByEq sameAtom declarations
  where
  declarations = styles >>= \(Style entries) -> resolve entries
  sameAtom left right = className left == className right

-- | Keep the last declaration for each property and condition.
resolve :: Array Declaration -> Array Declaration
resolve = Array.reverse <<< Array.nubByEq sameSlot <<< Array.reverse
  where
  sameSlot left right = left.property == right.property && left.condition == right.condition

className :: Declaration -> String
className { condition, property, value } = atomName $ conditionKey condition <> "|" <> property <> ":" <> value

renderDeclaration :: Declaration -> String
renderDeclaration declaration = case declaration.condition of
  Base -> ruleText ""
  InState state -> ruleText $ stateSelector state
  Under preference -> "@media " <> mediaQuery preference <> "{" <> ruleText "" <> "}"
  where
  ruleText suffix = "." <> className declaration <> suffix <> "{" <> declaration.property <> ":" <> declaration.value <> "}"

conditionKey :: Condition -> String
conditionKey = case _ of
  Base -> "base"
  InState state -> "state:" <> stateSelector state
  Under preference -> "media:" <> mediaQuery preference

stateSelector :: State -> String
stateSelector = case _ of
  Hover -> ":hover"
  Active -> ":active"
  FocusVisible -> ":focus-visible"
  Disabled -> ":disabled"
  Checked -> ":checked"
  Open -> "[data-open=true]"
  Selected -> "[data-active=true]"
  Invalid -> "[aria-invalid=true]"

mediaQuery :: Preference -> String
mediaQuery = case _ of
  Dark -> "(prefers-color-scheme:dark)"
  ReducedMotion -> "(prefers-reduced-motion:reduce)"
  MoreContrast -> "(prefers-contrast:more)"
  ForcedColors -> "(forced-colors:active)"
