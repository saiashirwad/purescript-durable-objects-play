module Test.Chat.Main where

import Prelude

import Ai (Finish(..), Message(..))
import Ai.Model as Model
import Chat.Room (PostError(..), ReactError(..), RoomEvents)
import Chat.Room as ChatRoom
import Chat.Room.Images as Images
import Chat.Room.Migrations (LegacyMessage)
import Chat.Room.Store as Store
import Chat.Room.Live (roomLive, roomLiveWith)
import Chat.Session as BrowserSession
import Chat.Page.Composer as Composer
import Chat.Page.Messages as Messages
import Cloudflare.Durable (Object, Signal(..))
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Codec (codec)
import Cloudflare.Durable.Core (Live(..))
import Cloudflare.Durable.Rpc (NoError, Rpc, RpcFailure(..), method)
import Cloudflare.Durable.Sql (Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Runtime (liftRuntime)
import Cloudflare.Durable.Simulator as Simulator
import Cloudflare.Durable.Storage as Storage
import Cloudflare.Worker as Worker
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Array (length)
import Data.Either (Either(..), fromRight, isLeft)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits as CodeUnits
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Foreign.Object as Object

main :: Effect Unit
main = launchAff_ do
  rooms <- Simulator.simulate roomLive

  check "a room keeps its messages in order" $ succeeds do
    id <- liftAff $ Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    first <- chat.post { author: "ann", text: "hello", images: [], replyTo: Nothing }
    second <- chat.post { author: "bob", text: "hi", images: [], replyTo: Nothing }
    snapshot <- Rpc.infallible $ chat.snapshot unit
    pure $ (_.id <$> snapshot.messages) == [ messageId 1, messageId 2 ] && first.id == messageId 1 && ChatRoom.printAuthor second.author == "bob"

  check "a blank post is a domain error" do
    let chat = Durable.getByName rooms "validation"
    result <- Rpc.run $ chat.post { author: "ann", text: "   ", images: [], replyTo: Nothing }
    pure $ result == Left (DomainError TextRequired)
  check "usernames follow mention syntax and reserve the assistant name" do
    let accepted = ChatRoom.printUserName <$> ChatRoom.mkUserName " ann-1 "
    invalid <- Rpc.run $ (Durable.getByName rooms "invalid-user").post { author: "Sai Ashirwad", text: "hi", images: [], replyTo: Nothing }
    reserved <- Rpc.run $ (Durable.getByName rooms "reserved-user").post { author: "AI", text: "hi", images: [], replyTo: Nothing }
    pure $ accepted == Right "ann-1"
      && invalid == Left (DomainError AuthorInvalid)
      && reserved == Left (DomainError AuthorReserved)
  check "semantic ids and authors validate their wire values" do
    let invalidMessage = CA.decode (codec :: CA.JsonCodec ChatRoom.MessageId) $ J.fromNumber 0.0
    let invalidImage = CA.decode (codec :: CA.JsonCodec ChatRoom.ImageId) $ J.fromNumber (-1.0)
    let invalidAuthor = CA.decode (codec :: CA.JsonCodec ChatRoom.Author) $ J.fromString "Sai Ashirwad"
    let assistant = CA.decode (codec :: CA.JsonCodec ChatRoom.Author) $ J.fromString "AI"
    pure
      $ isLeft invalidMessage
          && isLeft invalidImage
          && isLeft invalidAuthor
          && map ChatRoom.printAuthor assistant == Right "ai"

  check "nullary domain errors reject an unexpected id" do
    let
      malformed = J.fromObject $ Object.fromFoldable
        [ Tuple "tag" $ J.fromString "TextRequired"
        , Tuple "id" $ J.fromNumber 1.0
        ]
    pure case CA.decode (codec :: CA.JsonCodec PostError) malformed of
      Left _ -> true
      Right _ -> false

  check "sockets get posts and absolute presence" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    annLog <- liftEffect $ Ref.new []
    bobLog <- liftEffect $ Ref.new []
    let record ref signal = Ref.modify_ (_ <> [ describe signal ]) ref
    _ <- liftEffect $ Durable.listen rooms id "ann" (record annLog)
    stopBob <- liftEffect $ Durable.listen rooms id "bob" (record bobLog)
    _ <- Rpc.run $ chat.typing "bob"
    _ <- Rpc.run $ chat.post { author: "ann", text: "hi", images: [], replyTo: Nothing }
    current <- Rpc.run $ chat.snapshot unit
    liftEffect stopBob
    after <- Rpc.run $ chat.snapshot unit
    ann <- liftEffect $ Ref.read annLog
    bob <- liftEffect $ Ref.read bobLog
    pure $ ann == [ "opened", "presence [\"ann\"]", "presence [\"ann\",\"bob\"]", "typing bob", "message hi", "presence [\"ann\"]" ]
      && bob == [ "opened", "presence [\"ann\",\"bob\"]", "typing bob", "message hi", "closed" ]
      && map _.presence current == Right [ "ann", "bob" ]
      && map _.presence after == Right [ "ann" ]

  check "closing one of two same-name sockets keeps that name present" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    firstLog <- liftEffect $ Ref.new []
    secondLog <- liftEffect $ Ref.new []
    stopFirst <- liftEffect $ Durable.listen rooms id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) firstLog
    _ <- Rpc.run $ chat.snapshot unit
    stopSecond <- liftEffect $ Durable.listen rooms id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) secondLog
    _ <- Rpc.run $ chat.snapshot unit
    liftEffect stopFirst
    after <- Rpc.run $ chat.snapshot unit
    seen <- liftEffect $ Ref.read secondLog
    liftEffect stopSecond
    pure $ map _.presence after == Right [ "ann" ]
      && Array.length (Array.filter (_ == "presence [\"ann\"]") seen) == 2

  check "unique ids do not collide with names" $ succeeds do
    id <- liftAff $ Durable.newUniqueId rooms
    let byId = Durable.get rooms id
    let byName = Durable.getByName rooms (Durable.idToString id)
    let session = BrowserSession.open id
    _ <- byId.post { author: "ann", text: "only here", images: [], replyTo: Nothing }
    other <- Rpc.infallible $ byName.snapshot unit
    pure
      $ length other.messages == 0
          && Just id == Just (Durable.idFromString rooms (Durable.idToString id))
          && BrowserSession.fromRoute (BrowserSession.route id) == Just id
          && session.imageUrl (imageId 1) == "/rpc/Room/id/" <> BrowserSession.printRoomId id <> "/http/image/1"

  check "room migration imports legacy keys once and deletes both" do
    let current = (chatMessage 7 "ann" 7.0 Nothing) { text = "current", images = [ imageId 4 ], mentions = [], reactions = [ { emoji: "👍", by: [ "bob" ] } ] }
    currentRooms <- Simulator.simulate $ seededStoreLive false (Just [ current ]) (Just [ { id: 1, author: "old", text: "ignored", sentAt: 1.0 } ])
    currentId <- Durable.newUniqueId currentRooms
    currentInspection <- Rpc.run $ (Durable.get currentRooms currentId).inspect unit
    legacyRooms <- Simulator.simulate $ seededStoreLive false Nothing (Just [ { id: 3, author: "old", text: "hello @ann", sentAt: 3.0 } ])
    legacyId <- Durable.newUniqueId legacyRooms
    legacyInspection <- Rpc.run $ (Durable.get legacyRooms legacyId).inspect unit
    pure
      $ (map (\result -> map _.text result.messages) currentInspection == Right [ "current" ])
          && (map (\result -> map _.reactions result.messages) currentInspection == Right [ [ { emoji: "👍", by: [ "bob" ] } ] ])
          && (map (\result -> result.currentPresent || result.legacyPresent) currentInspection == Right false)
          && (map (\result -> map _.mentions result.messages) legacyInspection == Right [ [ "ann" ] ])
          && (map (\result -> result.currentPresent || result.legacyPresent) legacyInspection == Right false)

  timeline <- Simulator.clock

  check "a post mentioning @ai is answered from the alarm, with a tool call" do
    model <- Model.scripted
      [ { message: Assistant { text: Nothing, toolCalls: [ { id: "c1", name: "members", arguments: J.jsonEmptyObject } ] }, finish: ToolCalls, usage: Nothing }
      , { message: Assistant { text: Just "hello ann, just us two", toolCalls: [] }, finish: Stop, usage: Nothing }
      ]
    bots <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      timeline
      (roomLiveWith \_ -> model)
    id <- Durable.newUniqueId bots
    logs <- liftEffect $ Ref.new []
    _ <- liftEffect $ Durable.listen bots id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) logs
    _ <- Rpc.run $ (Durable.get bots id).post { author: "ann", text: "hey @ai, who is here?", images: [], replyTo: Nothing }
    Simulator.advance timeline (Milliseconds 0.0)
    seen <- liftEffect $ Ref.read logs
    snapshot <- Rpc.run $ (Durable.get bots id).snapshot unit
    pure $ seen == [ "opened", "presence [\"ann\"]", "message hey @ai, who is here?", "typing ai", "message hello ann, just us two" ]
      && ((map (map (ChatRoom.printAuthor <<< _.author) <<< _.messages) snapshot) == Right [ "ann", "ai" ])

  check "assistant triggers use parsed exact mentions" do
    quietModel <- Model.scripted []
    quietRooms <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      timeline
      (roomLiveWith \_ -> quietModel)
    id <- Durable.newUniqueId quietRooms
    let chat = Durable.get quietRooms id
    _ <- Rpc.run $ chat.post
      { author: "ann"
      , text: "hello @aiden and `@ai`\n```\n@ai\n```"
      , images: []
      , replyTo: Nothing
      }
    Simulator.advance timeline (Milliseconds 0.0)
    snapshot <- Rpc.run $ chat.snapshot unit
    pure $ map (map (ChatRoom.printAuthor <<< _.author) <<< _.messages) snapshot == Right [ "ann" ]

  check "assistant matching ignores case and keeps only the latest pending reply" do
    model <- Model.scripted
      [ { message: Assistant { text: Just "latest", toolCalls: [] }, finish: Stop, usage: Nothing } ]
    latestRooms <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      timeline
      (roomLiveWith \_ -> model)
    id <- Durable.newUniqueId latestRooms
    let chat = Durable.get latestRooms id
    _ <- Rpc.run $ chat.post { author: "ann", text: "first @AI", images: [], replyTo: Nothing }
    _ <- Rpc.run $ chat.post { author: "bob", text: "second @ai", images: [], replyTo: Nothing }
    Simulator.advance timeline (Milliseconds 0.0)
    snapshot <- Rpc.run $ chat.snapshot unit
    pure $ map (map _.replyTo <<< _.messages) snapshot == Right [ Nothing, Nothing, Just (messageId 2) ]
  check "assistant retries transient failures and emits one reply" do
    attempts <- liftEffect $ Ref.new 0
    let
      flaky = Model.Model \_ -> do
        attempt <- liftEffect do
          current <- Ref.read attempts
          Ref.write (current + 1) attempts
          pure $ current + 1
        pure
          if attempt == 1 then Left $ Model.Transport "private provider detail"
          else Right { message: Assistant { text: Just "recovered", toolCalls: [] }, finish: Stop, usage: Nothing }
    retryRooms <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      timeline
      (roomLiveWith \_ -> flaky)
    id <- Durable.newUniqueId retryRooms
    let chat = Durable.get retryRooms id
    _ <- Rpc.run $ chat.post { author: "ann", text: "retry this @ai", images: [], replyTo: Nothing }
    Simulator.advance timeline (Milliseconds 0.0)
    beforeRetry <- Rpc.run $ chat.snapshot unit
    Simulator.advance timeline (Milliseconds 1000.0)
    afterRetry <- Rpc.run $ chat.snapshot unit
    Simulator.advance timeline (Milliseconds 32000.0)
    afterExtraAlarm <- Rpc.run $ chat.snapshot unit
    attempted <- liftEffect $ Ref.read attempts
    pure
      $ map (map _.text <<< _.messages) beforeRetry == Right [ "retry this @ai" ]
          && map (map _.text <<< _.messages) afterRetry == Right [ "retry this @ai", "recovered" ]
          && map (map _.text <<< _.messages) afterExtraAlarm == Right [ "retry this @ai", "recovered" ]
          && attempted == 2

  check "image cleanup does not wake assistant retries early" do
    isolationTimeline <- Simulator.clock
    attempts <- liftEffect $ Ref.new 0
    let
      flaky = Model.Model \_ -> do
        attempt <- liftEffect $ Ref.modify (_ + 1) attempts
        pure
          if attempt == 1 then Left $ Model.Transport "try again"
          else Right { message: Assistant { text: Just "recovered", toolCalls: [] }, finish: Stop, usage: Nothing }
    isolationRooms <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      isolationTimeline
      (roomLiveWith \_ -> flaky)
    id <- Durable.newUniqueId isolationRooms
    let
      chat = Durable.get isolationRooms id
      png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
    _ <- Durable.http isolationRooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    Simulator.advance isolationTimeline (Milliseconds 86399999.0)
    _ <- Rpc.run $ chat.post { author: "ann", text: "retry @ai", images: [], replyTo: Nothing }
    Simulator.advance isolationTimeline (Milliseconds 0.0)
    Simulator.advance isolationTimeline (Milliseconds 2.0)
    early <- Rpc.run $ chat.snapshot unit
    earlyAttempts <- liftEffect $ Ref.read attempts
    Simulator.advance isolationTimeline (Milliseconds 998.0)
    retried <- Rpc.run $ chat.snapshot unit
    retriedAttempts <- liftEffect $ Ref.read attempts
    pure
      $ map (map _.text <<< _.messages) early == Right [ "retry @ai" ]
          && earlyAttempts == 1
          && map (map _.text <<< _.messages) retried == Right [ "retry @ai", "recovered" ]
          && retriedAttempts == 2

  check "assistant failures hide provider details and do not retry permanent errors" do
    attempts <- liftEffect $ Ref.new 0
    let
      denied = Model.Model \_ -> do
        liftEffect $ Ref.modify_ (_ + 1) attempts
        pure $ Left $ Model.Rejected { status: 401, body: "private provider detail" }
    deniedRooms <- Simulator.simulateWith
      (Simulator.noContainer { variables = Map.singleton "DEEPSEEK_API_KEY" "test-key" })
      timeline
      (roomLiveWith \_ -> denied)
    id <- Durable.newUniqueId deniedRooms
    let chat = Durable.get deniedRooms id
    _ <- Rpc.run $ chat.post { author: "ann", text: "answer @ai", images: [], replyTo: Nothing }
    Simulator.advance timeline (Milliseconds 0.0)
    Simulator.advance timeline (Milliseconds 32000.0)
    snapshot <- Rpc.run $ chat.snapshot unit
    attempted <- liftEffect $ Ref.read attempts
    pure
      $ map (map _.text <<< _.messages) snapshot == Right [ "answer @ai", "I could not answer right now." ]
          && map (map _.replyTo <<< _.messages) snapshot == Right [ Nothing, Just (messageId 1) ]
          && attempted == 1

  check "replies must point at a real message; mentions are recorded" do
    let chat = Durable.getByName rooms "threads"
    first <- Rpc.run $ chat.post { author: "ann", text: "hello @bob", images: [], replyTo: Nothing }
    bad <- Rpc.run $ chat.post { author: "bob", text: "??", images: [], replyTo: Just (messageId 99) }
    good <- Rpc.run $ chat.post { author: "bob", text: "hi!", images: [], replyTo: Just (messageId 1) }
    missing <- Rpc.run $ chat.post { author: "bob", text: "pic", images: [ imageId 42 ], replyTo: Nothing }
    pure $ map _.mentions first == Right [ "bob" ]
      && bad == Left (DomainError (NoSuchReply (messageId 99)))
      && map _.replyTo good == Right (Just (messageId 1))
      && missing == Left (DomainError (NoSuchImage (imageId 42)))

  check "reactions toggle per person and broadcast an update" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    logs <- liftEffect $ Ref.new []
    _ <- liftEffect $ Durable.listen rooms id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) logs
    _ <- Rpc.run $ chat.post { author: "ann", text: "vote", images: [], replyTo: Nothing }
    a <- Rpc.run $ chat.react { id: messageId 1, emoji: "👍", by: "ann" }
    b <- Rpc.run $ chat.react { id: messageId 1, emoji: "👍", by: "bob" }
    c <- Rpc.run $ chat.react { id: messageId 1, emoji: "👍", by: "ann" }
    d <- Rpc.run $ chat.react { id: messageId 1, emoji: "👍", by: "bob" }
    none <- Rpc.run $ chat.react { id: messageId 7, emoji: "👍", by: "bob" }
    seen <- liftEffect $ Ref.read logs
    pure $ map _.reactions a == Right [ { emoji: "👍", by: [ "ann" ] } ]
      && map _.reactions b == Right [ { emoji: "👍", by: [ "ann", "bob" ] } ]
      && map _.reactions c == Right [ { emoji: "👍", by: [ "bob" ] } ]
      && map _.reactions d == Right []
      && none == Left (DomainError (NoSuchMessage (messageId 7)))
      && Array.drop 3 seen == [ "updated [\"👍×1\"]", "updated [\"👍×2\"]", "updated [\"👍×1\"]", "updated []" ]

  check "reactions normalize input and require a reactor" do
    let chat = Durable.getByName rooms "reaction-validation"
    _ <- Rpc.run $ chat.post { author: "ann", text: "vote", images: [], replyTo: Nothing }
    normalized <- Rpc.run $ chat.react { id: messageId 1, emoji: " 👍 ", by: " ann " }
    blankReactor <- Rpc.run $ chat.react { id: messageId 1, emoji: "👍", by: "   " }
    pure $ map _.reactions normalized == Right [ { emoji: "👍", by: [ "ann" ] } ]
      && blankReactor == Left (DomainError ReactorRequired)

  check "image uploads enforce MIME, signatures, size, and safe serving" do
    id <- Durable.newUniqueId rooms
    let png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
    uploaded <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    body <- Worker.responseText uploaded
    text <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "text/plain", base64: "aGk=" }
    svg <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/svg+xml", base64: "PHN2Zz48L3N2Zz4=" }
    mismatch <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: "aGk=" }
    let oversized = "iVBORw0KGgoA" <> CodeUnits.fromCharArray (Array.replicate Images.maxImageChars 'A')
    tooLarge <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: oversized }
    served <- Durable.http rooms id $ Worker.requestTo "http://room/image/1"
    missing <- Durable.http rooms id $ Worker.requestTo "http://room/image/9"
    posted <- Rpc.run $ (Durable.get rooms id).post { author: "ann", text: "", images: [ imageId 1 ], replyTo: Nothing }
    pure $ Worker.status uploaded == 200 && body == "{\"id\":1}"
      && Worker.status text == 415
      && Worker.status svg == 415
      && Worker.status mismatch == 415
      && Worker.status tooLarge == 413
      && Worker.status served == 200
      && Worker.responseHeader served "x-content-type-options" == Just "nosniff"
      && Worker.status missing == 404
      && map _.images posted == Right [ imageId 1 ]

  check "abandoned uploads expire while attached images remain" do
    mediaTimeline <- Simulator.clock
    mediaRooms <- Simulator.simulateOn mediaTimeline roomLive
    id <- Durable.newUniqueId mediaRooms
    let png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
    _ <- Durable.http mediaRooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    _ <- Durable.http mediaRooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    _ <- Rpc.run $ (Durable.get mediaRooms id).post { author: "ann", text: "", images: [ imageId 2 ], replyTo: Nothing }
    Simulator.advance mediaTimeline (Milliseconds 86400001.0)
    freshUpload <- Durable.http mediaRooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    abandoned <- Durable.http mediaRooms id $ Worker.requestTo "http://room/image/1"
    attached <- Durable.http mediaRooms id $ Worker.requestTo "http://room/image/2"
    fresh <- Durable.http mediaRooms id $ Worker.requestTo "http://room/image/3"
    pure $ Worker.status abandoned == 404 && Worker.status attached == 200 && Worker.status freshUpload == 200 && Worker.status fresh == 200

  check "a matched fetch hook stops later hooks" do
    layered <- Simulator.simulate $ withLiveHooks
      (Durable.fetchHook \_ -> pure $ Just $ Worker.text 418 "later hook")
      roomLive
    id <- Durable.newUniqueId layered
    missing <- Durable.http layered id $ Worker.requestTo "http://room/image/9"
    missingBody <- Worker.responseText missing
    unmatched <- Durable.http layered id $ Worker.requestTo "http://room/other"
    pure $ Worker.status missing == 404
      && missingBody == "no such image"
      && Worker.status unmatched == 418

  check "SQL retention stays bounded and removes old image rows" do
    let text = CodeUnits.fromCharArray $ Array.replicate ChatRoom.maxTextLength 'x'
    let
      stored = Array.range 1 500 <#> \id ->
        (chatMessage id "ann" 1.0 Nothing) { images = if id == 1 then [ imageId 1 ] else [] }
    retainedRooms <- Simulator.simulate $ seededStoreLive true (Just stored) Nothing
    id <- Durable.newUniqueId retainedRooms
    posted <- Rpc.run $ (Durable.get retainedRooms id).post { author: testAuthor "ann", text, images: [], replyTo: Nothing }
    inspection <- Rpc.run $ (Durable.get retainedRooms id).inspect unit
    removedImage <- Durable.http retainedRooms id $ Worker.requestTo "http://room/image/1"
    replacement <- Durable.http retainedRooms id $ Worker.requestWith
      { url: "http://room/image", method: "POST", contentType: "image/png", base64: "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=" }
    replacementBody <- Worker.responseText replacement
    pure $ map _.id posted == Right (messageId 501)
      && (map (Array.length <<< _.messages) inspection == Right 500)
      && (map (map _.id <<< Array.head <<< _.messages) inspection == Right (Just (messageId 2)))
      && (map (map _.id <<< Array.last <<< _.messages) inspection == Right (Just (messageId 501)))
      && (map _.currentPresent inspection == Right false)
      && Worker.status removedImage == 404
      && replacementBody == "{\"id\":2}"

  check "threaded marks only close messages from the same author" do
    let
      flags =
        Messages.threaded
          [ chatMessage 1 "ann" 0.0 Nothing
          , chatMessage 2 "ann" 1000.0 Nothing
          , chatMessage 3 "ann" 2000.0 (Just 1)
          , chatMessage 4 "bob" 3000.0 Nothing
          , chatMessage 5 "bob" 400000.0 Nothing
          ] <#> \(Tuple position _) -> position
    pure $ flags ==
      [ Messages.StartsThread
      , Messages.ContinuesThread
      , Messages.StartsThread
      , Messages.StartsThread
      , Messages.StartsThread
      ]

  check "suggestions match names without case and exclude the author" do
    let room = { draft: "hello\t@b", members: [ "ann", "Bob", "bob", "bert" ], messages: Map.empty }
    pure $ Composer.suggestions "ann" room == [ "Bob", "bert" ]

  check "mention completion keeps whitespace before the active mention" do
    pure $ Composer.replaceLastWord "@Bob " "hello @b" == "hello @Bob "
      && Composer.replaceLastWord "@Bob " "@b" == "@Bob "
      && Composer.replaceLastWord "@Bob " "hello\n@b" == "hello\n@Bob "

  log "All chat tests passed."

chatMessage :: Int -> String -> Number -> Maybe Int -> ChatRoom.Message
chatMessage id author sentAt replyTo =
  { id: messageId id
  , author: testAuthor author
  , text: "message"
  , images: []
  , replyTo: messageId <$> replyTo
  , mentions: []
  , reactions: []
  , sentAt
  }

messageId :: Int -> ChatRoom.MessageId
messageId = ChatRoom.MessageId

imageId :: Int -> ChatRoom.ImageId
imageId = ChatRoom.ImageId

testAuthor :: String -> ChatRoom.Author
testAuthor = fromRight ChatRoom.Assistant <<< ChatRoom.mkAuthor

withLiveHooks
  :: forall name api events
   . Durable.Hooks
  -> Durable.Live name api events
  -> Durable.Live name api events
withLiveHooks extra (Live live) =
  Live (live { activate = map (map (_ `Durable.withHooks` extra)) live.activate })

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name

succeeds :: forall e. Show e => Rpc e Boolean -> Aff Boolean
succeeds call = Rpc.run call >>= case _ of
  Right passed -> pure passed
  Left failure -> liftEffect $ throw $ "unexpected failure: " <> show failure

type StoreTestApi =
  ( inspect :: Unit -> Rpc NoError { messages :: Array ChatRoom.Message, currentPresent :: Boolean, legacyPresent :: Boolean }
  , post :: ChatRoom.AcceptedMessage -> Rpc NoError ChatRoom.Message
  )

storeTestObject :: Object "StoreTest" StoreTestApi ()
storeTestObject = Durable.object { inspect: method, post: method }

currentMessagesKey :: Storage.Key (Array ChatRoom.Message)
currentMessagesKey = Storage.key "messages.v2"

legacyMessagesKey :: Storage.Key (Array LegacyMessage)
legacyMessagesKey = Storage.key "messages"

seededStoreLive :: Boolean -> Maybe (Array ChatRoom.Message) -> Maybe (Array LegacyMessage) -> Live "StoreTest" StoreTestApi ()
seededStoreLive seedImage current legacy = Durable.implementWith storeTestObject $ ado
  state <- Durable.state
  in
    do
      Sql.execute state createTestImages unit
      when seedImage $ Sql.execute state insertTestImage unit
      for_ current $ Storage.put state currentMessagesKey
      for_ legacy $ Storage.put state legacyMessagesKey
      images <- Images.open state
      store <- Store.open state
      pure $
        Durable.handlers
          { inspect: \_ -> do
              messages <- liftRuntime $ Store.snapshot store
              currentPresent <- liftRuntime $ isJust <$> Storage.get state currentMessagesKey
              legacyPresent <- liftRuntime $ isJust <$> Storage.get state legacyMessagesKey
              pure { messages, currentPresent, legacyPresent }
          , post: liftRuntime <<< Store.post store
          }
          `Durable.withHooks` Images.hooks images

createTestImages :: Statement Unit Unit
createTestImages = Sql.statement
  "CREATE TABLE IF NOT EXISTS images (id INTEGER PRIMARY KEY AUTOINCREMENT, mime TEXT NOT NULL, data TEXT NOT NULL, uploaded_at REAL NOT NULL, attached_at REAL)"
  Sql.noParams
  (pure unit)

insertTestImage :: Statement Unit Unit
insertTestImage = Sql.statement
  "INSERT INTO images (id, mime, data, uploaded_at, attached_at) VALUES (1, 'image/png', 'iVBORw0KGgo=', 0, 0)"
  Sql.noParams
  (pure unit)

describe :: Signal (Variant RoomEvents) -> String
describe = case _ of
  Opened -> "opened"
  Closed -> "closed"
  Garbled why -> "garbled " <> why
  Delivered event -> event # match
    { message: \message -> "message " <> message.text
    , updated: \message -> "updated " <> show (message.reactions <#> \reaction -> reaction.emoji <> "×" <> show (Array.length reaction.by))
    , presence: \names -> "presence " <> show names
    , typing: ("typing " <> _)
    }
