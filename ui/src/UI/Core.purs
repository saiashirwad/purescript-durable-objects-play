module UI.Core
  ( Tone(..)
  , Size(..)
  , Choice
  , toneName
  , sizeName
  , bool
  , dataAttr
  , nextEnabled
  , firstEnabled
  , lastEnabled
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Tone = Neutral | Accent | Danger | Quiet

derive instance Eq Tone

data Size = Small | Medium | Large

derive instance Eq Size

-- | One option of a select or radio group.
type Choice =
  { value :: String
  , label :: String
  , disabled :: Boolean
  }

toneName :: Tone -> String
toneName = case _ of
  Neutral -> "neutral"
  Accent -> "accent"
  Danger -> "danger"
  Quiet -> "quiet"

sizeName :: Size -> String
sizeName = case _ of
  Small -> "small"
  Medium -> "medium"
  Large -> "large"

bool :: Boolean -> String
bool value = if value then "true" else "false"

-- | `data-name="value"`: the stable hooks a page may style or test against.
dataAttr :: forall r i. String -> String -> HH.IProp r i
dataAttr name = HP.attr $ HH.AttrName $ "data-" <> name

-- Roving focus: which item takes the tab stop next, skipping disabled ones.

nextEnabled :: forall r. Int -> Array { disabled :: Boolean | r } -> Int -> Int
nextEnabled direction items current
  | Array.null items = 0
  | otherwise = go 1 $ wrap (current + direction)
      where
      count = Array.length items
      wrap value = ((value `mod` count) + count) `mod` count
      go seen candidate
        | seen > count = current
        | otherwise = case Array.index items candidate of
            Just item | not item.disabled -> candidate
            _ -> go (seen + 1) $ wrap (candidate + direction)

firstEnabled :: forall r. Array { disabled :: Boolean | r } -> Int
firstEnabled = fromMaybe 0 <<< Array.findIndex (not <<< _.disabled)

lastEnabled :: forall r. Array { disabled :: Boolean | r } -> Int
lastEnabled = fromMaybe 0 <<< Array.findLastIndex (not <<< _.disabled)
