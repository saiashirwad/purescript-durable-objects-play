module UI.Internal.Dom
  ( showModal
  , closeDialog
  , isBackdropClick
  , showPopover
  , hidePopover
  , popoverOpen
  , onRef
  ) where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Unsafe.Reference (unsafeRefEq)
import Web.DOM.Element (matches)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Event (Event, currentTarget, target)
import Web.HTML (window)
import Web.HTML.HTMLDialogElement as Dialog
import Web.HTML.HTMLDocument (activeElement)
import Web.HTML.HTMLElement (HTMLElement, focus, toElement)
import Web.HTML.Window (document)

-- | Open a `<dialog>` as a modal. Hands back what had focus, for `closeDialog`.
showModal :: HTMLElement -> Effect (Maybe HTMLElement)
showModal element = case Dialog.fromHTMLElement element of
  Nothing -> pure Nothing
  Just dialog -> Dialog.open dialog >>=
    if _ then pure Nothing
    else do
      previous <- activeElement =<< document =<< window
      Dialog.showModal dialog
      pure previous

-- | Close a `<dialog>` and give focus back to `previous`.
closeDialog :: Maybe HTMLElement -> HTMLElement -> Effect Unit
closeDialog previous element = do
  for_ (Dialog.fromHTMLElement element) \dialog ->
    whenM (Dialog.open dialog) (Dialog.close Nothing dialog)
  traverse_ focus previous

-- | A click on the dialog itself, not on anything inside it.
isBackdropClick :: Event -> Boolean
isBackdropClick event = case target event, currentTarget event of
  Just clicked, Just listener -> unsafeRefEq clicked listener
  _, _ -> false

showPopover :: HTMLElement -> Effect Unit
showPopover element = unlessM (popoverOpen element) (showPopoverImpl element)

hidePopover :: HTMLElement -> Effect Unit
hidePopover element = whenM (popoverOpen element) (hidePopoverImpl element)

popoverOpen :: HTMLElement -> Effect Boolean
popoverOpen = matches (QuerySelector ":popover-open") <<< toElement

-- The Popover API has no PureScript binding yet.
foreign import showPopoverImpl :: HTMLElement -> Effect Unit
foreign import hidePopoverImpl :: HTMLElement -> Effect Unit

-- | Run a DOM effect on a ref, if it is mounted.
onRef :: forall st act slots out m. MonadEffect m => H.RefLabel -> (HTMLElement -> Effect Unit) -> H.HalogenM st act slots out m Unit
onRef ref act = H.getHTMLElementRef ref >>= traverse_ (liftEffect <<< act)
