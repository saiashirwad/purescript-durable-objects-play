module Test.Chat.Main where

import Prelude

import Ai (Finish(..), Message(..))
import Ai.Model as Model
import Chat.Room (PostError(..), ReactError(..), RoomEvents)
import Chat.Room as ChatRoom
import Chat.Room.Live (roomLive, roomLiveWith)
import Chat.Page.Composer as Composer
import Chat.Page.Messages as Messages
import Cloudflare.Durable (Signal(..))
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Codec (codec)
import Cloudflare.Durable.Core (Live(..))
import Cloudflare.Durable.Rpc (Rpc, RpcFailure(..))
import Cloudflare.Durable.Rpc as Rpc
import Cloudflare.Durable.Simulator as Simulator
import Cloudflare.Worker as Worker
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Array (length)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
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
    history <- Rpc.infallible $ chat.history unit
    pure $ (_.id <$> history) == [ 1, 2 ] && first.id == 1 && second.author == "bob"

  check "a blank post is a domain error" do
    let chat = Durable.getByName rooms "validation"
    result <- Rpc.run $ chat.post { author: "ann", text: "   ", images: [], replyTo: Nothing }
    pure $ result == Left (DomainError TextRequired)

  check "nullary domain errors reject an unexpected id" do
    let
      malformed = J.fromObject $ Object.fromFoldable
        [ Tuple "tag" $ J.fromString "TextRequired"
        , Tuple "id" $ J.fromNumber 1.0
        ]
    pure case CA.decode (codec :: CA.JsonCodec PostError) malformed of
      Left _ -> true
      Right _ -> false

  check "sockets get posts and presence; members tracks them" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    annLog <- liftEffect $ Ref.new []
    bobLog <- liftEffect $ Ref.new []
    let record ref signal = Ref.modify_ (_ <> [ describe signal ]) ref
    _ <- liftEffect $ Durable.listen rooms id "ann" (record annLog)
    stopBob <- liftEffect $ Durable.listen rooms id "bob" (record bobLog)
    _ <- Rpc.run $ chat.typing "bob"
    _ <- Rpc.run $ chat.post { author: "ann", text: "hi", images: [], replyTo: Nothing }
    members <- Rpc.run $ chat.members unit
    liftEffect stopBob
    after <- Rpc.run $ chat.members unit
    ann <- liftEffect $ Ref.read annLog
    bob <- liftEffect $ Ref.read bobLog
    pure $ ann == [ "opened", "joined ann", "joined bob", "typing bob", "message hi", "left bob" ]
      && bob == [ "opened", "joined bob", "typing bob", "message hi", "closed" ]
      && members == Right [ "ann", "bob" ]
      && after == Right [ "ann" ]

  check "unique ids do not collide with names" $ succeeds do
    id <- liftAff $ Durable.newUniqueId rooms
    let byId = Durable.get rooms id
    let byName = Durable.getByName rooms (Durable.idToString id)
    _ <- byId.post { author: "ann", text: "only here", images: [], replyTo: Nothing }
    other <- Rpc.infallible $ byName.history unit
    pure $ length other == 0 && Just id == Just (Durable.idFromString rooms (Durable.idToString id))

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
    history <- Rpc.run $ (Durable.get bots id).history unit
    pure $ seen == [ "opened", "joined ann", "message hey @ai, who is here?", "typing ai", "message hello ann, just us two" ]
      && ((map _.author <$> history) == Right [ "ann", "ai" ])

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
    history <- Rpc.run $ chat.history unit
    pure $ map (map _.author) history == Right [ "ann" ]

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
    history <- Rpc.run $ chat.history unit
    pure $ map (map _.replyTo) history == Right [ Nothing, Nothing, Just 2 ]

  check "replies must point at a real message; mentions are recorded" do
    let chat = Durable.getByName rooms "threads"
    first <- Rpc.run $ chat.post { author: "ann", text: "hello @bob", images: [], replyTo: Nothing }
    bad <- Rpc.run $ chat.post { author: "bob", text: "??", images: [], replyTo: Just 99 }
    good <- Rpc.run $ chat.post { author: "bob", text: "hi!", images: [], replyTo: Just 1 }
    missing <- Rpc.run $ chat.post { author: "bob", text: "pic", images: [ 42 ], replyTo: Nothing }
    pure $ map _.mentions first == Right [ "bob" ]
      && bad == Left (DomainError (NoSuchReply 99))
      && map _.replyTo good == Right (Just 1)
      && missing == Left (DomainError (NoSuchImage 42))

  check "reactions toggle per person and broadcast an update" do
    id <- Durable.newUniqueId rooms
    let chat = Durable.get rooms id
    logs <- liftEffect $ Ref.new []
    _ <- liftEffect $ Durable.listen rooms id "ann" \signal -> Ref.modify_ (_ <> [ describe signal ]) logs
    _ <- Rpc.run $ chat.post { author: "ann", text: "vote", images: [], replyTo: Nothing }
    a <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "ann" }
    b <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "bob" }
    c <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "ann" }
    d <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "bob" }
    none <- Rpc.run $ chat.react { id: 7, emoji: "👍", by: "bob" }
    seen <- liftEffect $ Ref.read logs
    pure $ map _.reactions a == Right [ { emoji: "👍", by: [ "ann" ] } ]
      && map _.reactions b == Right [ { emoji: "👍", by: [ "ann", "bob" ] } ]
      && map _.reactions c == Right [ { emoji: "👍", by: [ "bob" ] } ]
      && map _.reactions d == Right []
      && none == Left (DomainError (NoSuchMessage 7))
      && Array.drop 3 seen == [ "updated [\"👍×1\"]", "updated [\"👍×2\"]", "updated [\"👍×1\"]", "updated []" ]

  check "reactions normalize input and require a reactor" do
    let chat = Durable.getByName rooms "reaction-validation"
    _ <- Rpc.run $ chat.post { author: "ann", text: "vote", images: [], replyTo: Nothing }
    normalized <- Rpc.run $ chat.react { id: 1, emoji: " 👍 ", by: " ann " }
    blankReactor <- Rpc.run $ chat.react { id: 1, emoji: "👍", by: "   " }
    pure $ map _.reactions normalized == Right [ { emoji: "👍", by: [ "ann" ] } ]
      && blankReactor == Left (DomainError ReactorRequired)

  check "images round-trip through the room's fetch hook and validate on post" do
    id <- Durable.newUniqueId rooms
    let png = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
    uploaded <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "image/png", base64: png }
    body <- Worker.responseText uploaded
    rejected <- Durable.http rooms id $ Worker.requestWith { url: "http://room/image", method: "POST", contentType: "text/plain", base64: "aGk=" }
    served <- Durable.http rooms id $ Worker.requestTo "http://room/image/1"
    missing <- Durable.http rooms id $ Worker.requestTo "http://room/image/9"
    posted <- Rpc.run $ (Durable.get rooms id).post { author: "ann", text: "", images: [ 1 ], replyTo: Nothing }
    pure $ Worker.status uploaded == 200 && body == "{\"id\":1}" && Worker.status rejected == 415
      && Worker.status served == 200
      && Worker.status missing == 404
      && map _.images posted == Right [ 1 ]

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

  check "appendMessage owns message creation and history retention" do
    let
      existing = Array.range 1 500 <#> \id -> chatMessage id "ann" (Int.toNumber id) Nothing
      Tuple appended retained = ChatRoom.appendMessage 501.0
        { author: "bob", text: "hello @ann", images: [], replyTo: Just 500 }
        existing
    pure $ appended.id == 501
      && appended.mentions == [ "ann" ]
      && appended.reactions == []
      && Array.length retained == 500
      && map _.id (Array.head retained) == Just 2

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
  { id
  , author
  , text: "message"
  , images: []
  , replyTo
  , mentions: []
  , reactions: []
  , sentAt
  }

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

describe :: Signal (Variant RoomEvents) -> String
describe = case _ of
  Opened -> "opened"
  Closed -> "closed"
  Garbled why -> "garbled " <> why
  Delivered event -> event # match
    { message: \message -> "message " <> message.text
    , updated: \message -> "updated " <> show (message.reactions <#> \reaction -> reaction.emoji <> "×" <> show (Array.length reaction.by))
    , joined: ("joined " <> _)
    , left: ("left " <> _)
    , typing: ("typing " <> _)
    }
