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
  , names
  , inlineVars
  , render
  ) where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable (fold)
import Data.Int (base36, floor, toNumber, toStringAs)
import Data.Int.Bits (and, shl, xor, zshr)
import Data.Number as Number
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
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

-- | A short class name: FNV-1a of the key, in base 36.
atomName :: String -> String
atomName key = "u" <> digits (unsigned $ Array.foldl step offset $ toCharArray key)
  where
  offset = -2128831035 -- 2166136261, as a signed 32-bit int
  step hash char = imul (hash `xor` toCharCode char) 16777619
  unsigned hash = if hash < 0 then toNumber hash + 4294967296.0 else toNumber hash

-- | Multiply modulo 2^32. Plain `*` goes through a double and loses low bits.
imul :: Int -> Int -> Int
imul a b = al * bl + ((ah * bl + al * bh) `shl` 16)
  where
  ah = a `zshr` 16
  al = a `and` 65535
  bh = b `zshr` 16
  bl = b `and` 65535

-- | Base 36 of a whole number that may not fit in an `Int`.
digits :: Number -> String
digits = go ""
  where
  go acc value =
    let
      rest = Number.floor (value / 36.0)
      digit = toStringAs base36 (floor (value - rest * 36.0)) <> acc
    in
      if rest == 0.0 then digit else go digit rest

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
classes = map HH.ClassName <<< names

-- | Class names as plain strings, for elements whose `class` is an attribute
-- | because their `className` property is read-only (SVG).
names :: Style -> Array String
names (Style declarations) = className <$> resolve declarations

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
