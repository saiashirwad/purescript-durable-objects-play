module UI.Internal.Dom
  ( showModal
  , closeDialog
  , isBackdropClick
  , showPopover
  , hidePopover
  , popoverOpen
  , focusElement
  , onRef
  ) where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Web.Event.Event (Event)
import Web.HTML.HTMLElement (HTMLElement)

foreign import showModal :: HTMLElement -> Effect Unit
foreign import closeDialog :: HTMLElement -> Effect Unit
foreign import isBackdropClick :: Event -> Boolean
foreign import showPopover :: HTMLElement -> Effect Unit
foreign import hidePopover :: HTMLElement -> Effect Unit
foreign import popoverOpen :: HTMLElement -> Effect Boolean
foreign import focusElement :: HTMLElement -> Effect Unit

-- | Run a DOM effect on a ref, if it is mounted.
onRef :: forall st act slots out m. MonadEffect m => H.RefLabel -> (HTMLElement -> Effect Unit) -> H.HalogenM st act slots out m Unit
onRef ref act = H.getHTMLElementRef ref >>= traverse_ (liftEffect <<< act)
