module Chat.Page.Composer
  ( composer
  , sendable
  , mentionFocus
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
import Chat.Page.Types (App, ComposerAction(..), ComposerState, ComposerStatus(..), RoomToken, RoomView, modifyRoom, modifyRoomAt, withRoom)
import Chat.Room (ImageId, Message, MessageId, assistantName, describePostError, printAuthor, printImageId)
import Chat.Style (styles)
import Cloudflare.Durable.Rpc as Rpc
import Control.Promise (Promise, toAffE)
import Data.Array (filter, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Monoid (guard)
import Data.String (Pattern(..), stripPrefix)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
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
import Web.Clipboard.ClipboardEvent as Clipboard
import Web.Event.Event (preventDefault)
import Web.File.File (File)
import Web.File.FileList as FileList
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.HTMLElement (click, focus)
import Web.HTML.HTMLInputElement as InputElement
import Web.UIEvent.KeyboardEvent (key, toEvent)

foreign import uploadFiles :: String -> Array File -> Effect (Promise (Array ImageId))

composerRef :: H.RefLabel
composerRef = H.RefLabel "composer"

fileRef :: H.RefLabel
fileRef = H.RefLabel "file"

composer :: forall w. String -> RoomView -> HH.HTML w ComposerAction -> HH.HTML w ComposerAction
composer author room typing =
  HH.footer [ css styles.footer ]
    [ typing
    , replyChip room
    , attachmentStrip room
    , suggestionBar author room
    , HH.form [ css styles.composer, HE.onSubmit Submit, dataAttr "ui" "composer" ]
        [ Button.iconButton "Attach image" (Button.defaults { tone = Quiet, disabled = not (isEditing room) }) [ HP.title "Attach image", HE.onClick \_ -> Attach ]
            [ Icon.styled styles.largeIcon imageIcon ]
        , HH.input
            [ HP.ref fileRef
            , HP.type_ HP.InputFile
            , HP.attr (HH.AttrName "accept") "image/jpeg,image/png,image/webp,image/gif,image/avif"
            , HP.multiple true
            , HP.disabled $ not $ isEditing room
            , HP.style "display:none"
            , HE.onFileUpload SelectedFiles
            ]
        , Input.text (Input.defaults { disabled = not (isEditing room), styles = styles.input })
            [ HP.placeholder $ "Message · @" <> assistantName <> " to ask the assistant · markdown ok"
            , ARIA.label "Message"
            , HP.autofocus true
            , HP.autocomplete HP.AutocompleteOff
            , HP.value room.composer.draft
            , HP.ref composerRef
            , HE.onValueInput SetDraft
            , HE.onKeyDown KeyDown
            , HE.onPaste Pasted
            ]
        , Button.submit
            (Button.defaults { tone = Accent, disabled = not (isEditing room) || not (sendable room), busy = room.composer.status == Sending, styles = styles.send })
            [ HP.title "Send", ARIA.label "Send" ]
            [ Icon.render sendIcon ]
        ]
    , errorLine room.error
    ]

sendable :: RoomView -> Boolean
sendable room = not (blank room.composer.draft) || not (Array.null room.composer.attachments)

suggestionBar :: forall w. String -> RoomView -> HH.HTML w ComposerAction
suggestionBar author room = case suggestions author (suggestionInput room) of
  [] -> HH.text ""
  names -> HH.div [ css styles.suggestions, ARIA.label "Mention suggestions" ] $ names <#> \name ->
    Button.button (quiet { disabled = not (isEditing room), styles = styles.suggestion }) [ HE.onClick \_ -> PickMention name ]
      [ avatar styles.compactAvatar name, HH.text name ]

replyChip :: forall w. RoomView -> HH.HTML w ComposerAction
replyChip room = case room.composer.replyTo >>= \id -> Map.lookup id room.messages of
  Nothing -> HH.text ""
  Just parent ->
    HH.div [ css styles.replyChip ]
      [ Icon.render replyIcon
      , HH.span [ css styles.replyChipText ]
          [ HH.text "Replying to ", HH.strong_ [ HH.text $ printAuthor parent.author ], HH.text $ ": " <> String.take 60 (Markdown.plain parent.text) ]
      , Button.iconButton "Cancel reply" (quiet { disabled = not (isEditing room) }) [ HP.title "Cancel", HE.onClick \_ -> Reply Nothing ] [ HH.text "×" ]
      ]

attachmentStrip :: forall w. RoomView -> HH.HTML w ComposerAction
attachmentStrip room
  | Array.null room.composer.attachments && room.composer.status /= Uploading = HH.text ""
  | otherwise =
      HH.div [ css styles.attachments, ARIA.label "Image attachments" ] $
        (thumbnail <$> room.composer.attachments)
          <> guard (room.composer.status == Uploading) [ HH.span [ css styles.uploading, ARIA.role "status" ] [ HH.text "Uploading an image…" ] ]
      where
      thumbnail n = HH.span [ css styles.attachment ]
        [ HH.img [ css styles.thumbnail, HP.src (imageUrl room.id n), HP.alt "Image attachment preview" ]
        , Button.iconButton ("Remove attachment " <> show (printImageId n)) (small { disabled = not (isEditing room), styles = styles.remove }) [ HE.onClick \_ -> Detach n ] [ HH.text "×" ]
        ]

suggestions
  :: forall row
   . String
  -> { draft :: String, members :: Array String, messages :: Map.Map MessageId Message | row }
  -> Array String
suggestions author room = case mentionFocus room.draft of
  Just { query } ->
    take 6 $ filter (matches query) candidates
  _ -> []
  where
  candidates = Array.nubByEq sameName $ [ assistantName ] <> room.members <> (Array.fromFoldable room.messages <#> printAuthor <<< _.author)
  sameName left right = String.toLower left == String.toLower right
  matches query name = not (sameName name author) && isJust (stripPrefix (Pattern (String.toLower query)) (String.toLower name))

mentionFocus :: String -> Maybe { before :: String, query :: String }
mentionFocus draft = do
  let start = maybe 0 (_ + 1) $ Array.findLastIndex whitespace $ CodeUnits.toCharArray draft
  query <- stripPrefix (Pattern "@") $ CodeUnits.drop start draft
  pure { before: CodeUnits.take start draft, query }
  where
  whitespace character = character == ' ' || character == '\t' || character == '\n' || character == '\r'

handle :: forall m. MonadAff m => ComposerAction -> App m Unit
handle = case _ of
  SetDraft draft -> whileEditing \_ -> modifyComposer (_ { draft = draft }) *> pingTyping
  KeyDown event -> whileEditing \room -> do
    { author } <- H.get
    case key event, Array.head (suggestions author (suggestionInput room)) of
      "Tab", Just first -> do
        liftEffect $ preventDefault $ toEvent event
        handle $ PickMention first
      "Escape", _ -> modifyComposer (_ { replyTo = Nothing })
      _, _ -> pure unit
  PickMention name -> whileEditing \_ -> do
    modifyComposer \composerState -> composerState { draft = replaceLastWord ("@" <> name <> " ") composerState.draft }
    focusComposer
  Pasted event -> whileEditing \room -> case Clipboard.clipboardData event >>= DataTransfer.files of
    Nothing -> pure unit
    Just list -> do
      let files = FileList.items list
      unless (Array.null files) do
        liftEffect $ preventDefault $ Clipboard.toEvent event
        upload room.token $ uploadFiles (imageEndpoint room.id) files
  Attach -> whileEditing \_ -> H.getHTMLElementRef fileRef >>= traverse_ (liftEffect <<< click)
  SelectedFiles files -> whileEditing \room -> unless (Array.null files) do
    resetFileInput
    upload room.token $ uploadFiles (imageEndpoint room.id) files
  Detach n -> whileEditing \_ -> modifyComposer \composerState -> composerState { attachments = filter (_ /= n) composerState.attachments }
  Reply target -> whileEditing \_ -> modifyComposer (_ { replyTo = target }) *> focusComposer
  Submit event -> liftEffect (preventDefault event) *> submit

submit :: forall m. MonadAff m => App m Unit
submit = whileEditing \room -> when (sendable room) do
  { author } <- H.get
  let token = room.token
  let composerState = room.composer
  modifyRoomAt token \current ->
    if isEditing current then
      (mapComposer (_ { status = Sending }) current) { error = Nothing }
    else current
  outcome <- liftAff $ Rpc.run $ room.api.post
    { author
    , text: composerState.draft
    , images: composerState.attachments
    , replyTo: composerState.replyTo
    }
  withRoom \current -> when (current.token == token && current.composer.status == Sending) do
    modifyRoomAt token \latest ->
      if latest.composer.status /= Sending then latest
      else case outcome of
        Right _ -> mapComposer (_ { draft = "", attachments = [], replyTo = Nothing, status = Editing }) latest
        Left failure -> (mapComposer (_ { status = Editing }) latest) { error = Just $ Chat.describeFailure describePostError failure }
    focusComposer

pingTyping :: forall m. MonadAff m => App m Unit
pingTyping = withRoom \room -> unless (blank room.composer.draft) do
  at <- liftEffect nowMs
  when (at - room.typingSentAt > typingThrottle) do
    modifyRoomAt room.token _ { typingSentAt = at }
    { author } <- H.get
    void $ liftAff $ Rpc.run $ room.api.typing author

upload :: forall m. MonadAff m => RoomToken -> Effect (Promise (Array ImageId)) -> App m Unit
upload token go = do
  modifyRoomAt token \room ->
    if isEditing room then
      (mapComposer (_ { status = Uploading }) room) { error = Nothing }
    else room
  outcome <- liftAff $ attempt $ toAffE go
  withRoom \room -> when (room.token == token && room.composer.status == Uploading) do
    modifyRoomAt token \current ->
      if current.composer.status /= Uploading then current
      else case outcome of
        Left error -> (mapComposer (_ { status = Editing }) current) { error = Just $ "Upload failed: " <> message error }
        Right ids -> mapComposer (\composerState -> composerState { attachments = composerState.attachments <> ids, status = Editing }) current

whileEditing :: forall m. MonadAff m => (RoomView -> App m Unit) -> App m Unit
whileEditing use = withRoom \room -> when (isEditing room) $ use room

isEditing :: RoomView -> Boolean
isEditing room = room.composer.status == Editing

modifyComposer :: forall m. (ComposerState -> ComposerState) -> App m Unit
modifyComposer update = modifyRoom $ mapComposer update

mapComposer :: (ComposerState -> ComposerState) -> RoomView -> RoomView
mapComposer update room = room { composer = update room.composer }

suggestionInput :: RoomView -> { draft :: String, members :: Array String, messages :: Map.Map MessageId Message }
suggestionInput room = { draft: room.composer.draft, members: room.members, messages: room.messages }

focusComposer :: forall m. MonadAff m => App m Unit
focusComposer = H.getHTMLElementRef composerRef >>= traverse_ (liftEffect <<< focus)

resetFileInput :: forall m. MonadAff m => App m Unit
resetFileInput = H.getHTMLElementRef fileRef >>= traverse_ \element ->
  liftEffect $ traverse_ (InputElement.setValue "") $ InputElement.fromHTMLElement element

replaceLastWord :: String -> String -> String
replaceLastWord replacement draft = maybe draft (\{ before } -> before <> replacement) $ mentionFocus draft

errorLine :: forall w action. Maybe String -> HH.HTML w action
errorLine = maybe (HH.text "") \why -> Status.error [ HH.text why ]

typingThrottle :: Number
typingThrottle = 1500.0
