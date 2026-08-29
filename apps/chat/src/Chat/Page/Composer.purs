module Chat.Page.Composer
  ( composer
  , sendable
  , suggestions
  , replaceLastWord
  , handle
  , focusComposer
  ) where

import Prelude

import Chat.Client as Chat
import Chat.Page.Browser (nowMs)
import Chat.Page.Icons (imageIcon, replyIcon, sendIcon)
import Chat.Page.Shared (avatar, blank, imageEndpoint, imageUrl, quiet, small)
import Chat.Page.Types (App, ComposerAction(..), RoomView, inRoom, withRoom)
import Chat.Room (Message, assistantName)
import Chat.Style (styles)
import Cloudflare.Durable.Rpc as Rpc
import Control.Promise (Promise, toAffE)
import Data.Array (filter, last, length, take)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (guard)
import Data.String (Pattern(..), joinWith, split, stripPrefix)
import Data.String as String
import Effect (Effect)
import Effect.Aff (attempt, message)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Markdown as Markdown
import UI.Button as Button
import UI.Core (Tone(..), dataAttr)
import UI.Icon as Icon
import UI.Input as Input
import UI.Status as Status
import UI.Style (css)
import Web.Event.Event (Event, EventType(..), preventDefault)
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent (key, toEvent)

foreign import pickAndUpload :: String -> Effect (Promise (Array Int))
foreign import uploadPasted :: String -> Event -> Effect (Promise (Array Int))

composerRef :: H.RefLabel
composerRef = H.RefLabel "composer"

composer :: forall w. String -> RoomView -> HH.HTML w ComposerAction -> HH.HTML w ComposerAction
composer author room typing =
  HH.footer [ css styles.footer ]
    [ typing
    , replyChip room
    , attachmentStrip room
    , suggestionBar author room
    , HH.form [ css styles.composer, HE.onSubmit Submit, dataAttr "ui" "composer" ]
        [ Button.iconButton "Attach image" (Button.defaults { tone = Quiet }) [ HP.title "Attach image", HE.onClick \_ -> Attach ]
            [ Icon.styled styles.largeIcon imageIcon ]
        , Input.text (Input.defaults { disabled = room.sending, styles = styles.input })
            [ HP.placeholder $ "Message · @" <> assistantName <> " to ask the assistant · markdown ok"
            , ARIA.label "Message"
            , HP.autofocus true
            , HP.autocomplete HP.AutocompleteOff
            , HP.value room.draft
            , HP.ref composerRef
            , HE.onValueInput SetDraft
            , HE.onKeyDown KeyDown
            , HE.handler (EventType "paste") Pasted
            ]
        , Button.submit
            (Button.defaults { tone = Accent, disabled = room.sending || room.uploading || not (sendable room), busy = room.sending, styles = styles.send })
            [ HP.title "Send", ARIA.label "Send" ]
            [ Icon.render sendIcon ]
        ]
    , errorLine room.error
    ]

sendable :: RoomView -> Boolean
sendable room = not (blank room.draft) || not (Array.null room.attachments)

suggestionBar :: forall w. String -> RoomView -> HH.HTML w ComposerAction
suggestionBar author room = case suggestions author room of
  [] -> HH.text ""
  names -> HH.div [ css styles.suggestions, ARIA.label "Mention suggestions" ] $ names <#> \name ->
    Button.button (quiet { styles = styles.suggestion }) [ HE.onClick \_ -> PickMention name ]
      [ avatar styles.compactAvatar name, HH.text name ]

replyChip :: forall w. RoomView -> HH.HTML w ComposerAction
replyChip room = case room.replyTo >>= \id -> Map.lookup id room.messages of
  Nothing -> HH.text ""
  Just parent ->
    HH.div [ css styles.replyChip ]
      [ Icon.render replyIcon
      , HH.span [ css styles.replyChipText ]
          [ HH.text "Replying to ", HH.strong_ [ HH.text parent.author ], HH.text $ ": " <> String.take 60 (Markdown.plain parent.text) ]
      , Button.iconButton "Cancel reply" quiet [ HP.title "Cancel", HE.onClick \_ -> Reply Nothing ] [ HH.text "×" ]
      ]

attachmentStrip :: forall w. RoomView -> HH.HTML w ComposerAction
attachmentStrip room
  | Array.null room.attachments && not room.uploading = HH.text ""
  | otherwise =
      HH.div [ css styles.attachments, ARIA.label "Image attachments" ] $
        (thumbnail <$> room.attachments)
          <> guard room.uploading [ HH.span [ css styles.uploading, ARIA.role "status" ] [ HH.text "Uploading an image…" ] ]
      where
      thumbnail n = HH.span [ css styles.attachment ]
        [ HH.img [ css styles.thumbnail, HP.src (imageUrl room.id n), HP.alt "Image attachment preview" ]
        , Button.iconButton ("Remove attachment " <> show n) (small { styles = styles.remove }) [ HE.onClick \_ -> Detach n ] [ HH.text "×" ]
        ]

suggestions
  :: forall row
   . String
  -> { draft :: String, members :: Array String, messages :: Map.Map Int Message | row }
  -> Array String
suggestions author room = case last (split (Pattern " ") room.draft) >>= stripPrefix (Pattern "@") of
  Just partial | not (String.contains (Pattern "\n") partial) ->
    take 6 $ filter (\name -> name /= author && isJust (stripPrefix (Pattern (String.toLower partial)) (String.toLower name))) candidates
  _ -> []
  where
  candidates = Array.nub $ [ assistantName ] <> room.members <> (Array.fromFoldable room.messages <#> _.author)

handle :: forall m. MonadAff m => ComposerAction -> App m Unit
handle = case _ of
  SetDraft draft -> inRoom _ { draft = draft } *> pingTyping
  KeyDown event -> withRoom \room -> do
    { author } <- H.get
    case key event, Array.head (suggestions author room) of
      "Tab", Just first -> do
        liftEffect $ preventDefault $ toEvent event
        handle $ PickMention first
      "Escape", _ -> inRoom _ { replyTo = Nothing }
      _, _ -> pure unit
  PickMention name -> do
    inRoom \room -> room { draft = replaceLastWord ("@" <> name <> " ") room.draft }
    focusComposer
  Pasted event -> withRoom \room -> upload $ uploadPasted (imageEndpoint room.id) event
  Attach -> withRoom \room -> upload $ pickAndUpload (imageEndpoint room.id)
  Attached ids -> inRoom \room -> room { attachments = room.attachments <> ids, uploading = false }
  Detach n -> inRoom \room -> room { attachments = filter (_ /= n) room.attachments }
  Reply target -> inRoom _ { replyTo = target } *> focusComposer
  Submit event -> liftEffect (preventDefault event) *> submit

submit :: forall m. MonadAff m => App m Unit
submit = withRoom \room -> when (sendable room) do
  { author } <- H.get
  inRoom _ { sending = true, error = Nothing }
  outcome <- liftAff $ Rpc.run $ room.room.post { author, text: room.draft, images: room.attachments, replyTo: room.replyTo }
  inRoom case outcome of
    Right _ -> _ { sending = false, draft = "", attachments = [], replyTo = Nothing }
    Left failure -> _ { sending = false, error = Just $ Chat.describeFailure failure }
  focusComposer

pingTyping :: forall m. MonadAff m => App m Unit
pingTyping = withRoom \room -> unless (blank room.draft) do
  at <- liftEffect nowMs
  when (at - room.typingSentAt > typingThrottle) do
    inRoom _ { typingSentAt = at }
    { author } <- H.get
    void $ liftAff $ Rpc.run $ room.room.typing author

upload :: forall m. MonadAff m => Effect (Promise (Array Int)) -> App m Unit
upload go = do
  inRoom _ { uploading = true, error = Nothing }
  liftAff (attempt $ toAffE go) >>= either
    (\error -> inRoom _ { uploading = false, error = Just $ "Upload failed: " <> message error })
    (handle <<< Attached)

focusComposer :: forall m. MonadAff m => App m Unit
focusComposer = H.getHTMLElementRef composerRef >>= traverse_ (liftEffect <<< focus)

replaceLastWord :: String -> String -> String
replaceLastWord replacement draft =
  joinWith " " (Array.dropEnd 1 words) <> (if length words > 1 then " " else "") <> replacement
  where
  words = split (Pattern " ") draft

errorLine :: forall w action. Maybe String -> HH.HTML w action
errorLine = maybe (HH.text "") \why -> Status.error [ HH.text why ]

typingThrottle :: Number
typingThrottle = 1500.0
