module Chat.Page.Messages
  ( messageList
  , emptyRoom
  , messageItem
  , markdown
  , quickEmojis
  , threaded
  ) where

import Prelude

import Chat.Page.Browser (TimeFormatter)
import Chat.Page.Icons (replyIcon)
import Chat.Page.Shared (avatar, quiet)
import Chat.Room (Message, MessageId, isAssistant, printAuthor, printMessageId)
import Chat.Session (RoomSession)
import Chat.Style.Message (Continuity(..), Ownership(..), styles)
import Chat.Style.Message as MessageStyle
import Data.Array (elem, length, zip)
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import Data.String (Pattern(..), joinWith, stripPrefix)
import Data.String as String
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as ARIA
import Markdown (Block(..), Inline(..))
import Markdown as Markdown
import UI.Button as Button
import UI.Core (dataAttr)
import UI.Icon as Icon
import UI.Style (css)

type Actions action =
  { react :: MessageId -> String -> action
  , jumpTo :: MessageId -> action
  , reply :: Maybe MessageId -> action
  }

type MessageRoom row =
  { session :: RoomSession
  , messages :: Map.Map MessageId Message
  | row
  }

messageList :: forall action w row. Actions action -> TimeFormatter -> String -> MessageRoom row -> HH.HTML w action
messageList actions formatTime author room =
  HH.ol [ css styles.list, ARIA.label "Messages" ]
    if Map.isEmpty room.messages then [ emptyRoom ]
    else messageItem actions formatTime author room <$> threaded (Array.fromFoldable room.messages)

emptyRoom :: forall action w. HH.HTML w action
emptyRoom =
  HH.li [ css $ styles.message <> styles.empty ]
    [ HH.p [ css styles.emptyTitle ] [ HH.text "It's quiet in here" ]
    , HH.p [ css styles.muted ] [ HH.text "Share the link and say hello." ]
    ]

messageItem :: forall action w row. Actions action -> TimeFormatter -> String -> MessageRoom row -> Tuple Continuity Message -> HH.HTML w action
messageItem actions formatTime author room (Tuple continuity message) =
  HH.li
    [ HP.id $ "msg-" <> show (printMessageId message.id)
    , css $ MessageStyle.row { ownership, continuity }
    , dataAttr "ui" "message"
    ]
    $ guard (not mine) [ if continued then HH.span [ css styles.gutter, ARIA.hidden "true" ] [] else avatar (MessageStyle.avatar bot) authorName ]
        <> [ HH.div [ css styles.stack ] [ bubble, reactions, actionBar ] ]
  where
  continued = continuity == Continues
  ownership = if authorName == author then Own else Other
  mine = ownership == Own
  bot = isAssistant message.author
  mentioned = author `elem` message.mentions
  headless = mine || continued
  authorName = printAuthor message.author

  bubble = HH.div [ css $ MessageStyle.bubble { ownership, continuity, mentioned } ] $ fold
    [ guard (not headless) [ HH.span [ css styles.author ] [ HH.text authorName ] ]
    , foldMap (pure <<< quote) $ message.replyTo >>= \id -> Map.lookup id room.messages
    , markdown author ownership message.text
    , image <$> message.images
    , [ HH.span [ css styles.time ] [ HH.text $ formatTime message.sentAt ] ]
    ]

  quote parent =
    Button.button (quiet { styles = MessageStyle.quote ownership }) [ HE.onClick \_ -> actions.jumpTo parent.id ]
      [ HH.span [ css styles.quoteAuthor ] [ HH.text $ printAuthor parent.author ]
      , HH.span [ css $ MessageStyle.quoteText ownership ]
          [ HH.text $ String.take 120 $ Markdown.plain parent.text ]
      ]

  image n =
    HH.a [ css styles.imageLink, HP.href (room.session.imageUrl n), HP.target "_blank", HP.rel "noopener noreferrer" ]
      [ HH.img [ css styles.image, HP.src (room.session.imageUrl n), HP.alt $ authorName <> " attached an image" ] ]

  reactions
    | Array.null message.reactions = HH.text ""
    | otherwise = HH.div [ css $ MessageStyle.reactions ownership ] $ message.reactions <#> \{ emoji, by } ->
        Button.button (quiet { styles = MessageStyle.reaction $ author `elem` by })
          [ HP.title (joinWith ", " by), HE.onClick \_ -> actions.react message.id emoji ]
          [ HH.text emoji, HH.span [ css styles.reactionCount ] [ HH.text $ show (length by) ] ]

  actionBar =
    HH.div [ css $ MessageStyle.actions ownership, ARIA.label $ "Actions for " <> authorName <> "'s message", dataAttr "ui" "actions" ] $
      (quickEmojis <#> \emoji -> Button.iconButton ("React with " <> emoji) tiny [ HP.title emoji, HE.onClick \_ -> actions.react message.id emoji ] [ HH.text emoji ])
        <> [ Button.iconButton "Reply" tiny [ HP.title "Reply", HE.onClick \_ -> actions.reply (Just message.id) ] [ Icon.render replyIcon ] ]

  tiny = quiet { styles = styles.actionButton }

quickEmojis :: Array String
quickEmojis = [ "👍", "❤️", "😂", "🎉", "👀" ]

markdown :: forall action w. String -> Ownership -> String -> Array (HH.HTML w action)
markdown me ownership = map block <<< Markdown.parse
  where
  block = case _ of
    Paragraph xs -> HH.p [ css styles.paragraph ] (inline <$> xs)
    Heading 1 xs -> HH.h2 [ css styles.subheading ] (inline <$> xs)
    Heading 2 xs -> HH.h3 [ css styles.subheading ] (inline <$> xs)
    Heading _ xs -> HH.h4 [ css styles.subheading ] (inline <$> xs)
    Quote blocks -> HH.blockquote [ css styles.blockquote ] (block <$> blocks)
    Bullets items -> HH.ul [ css styles.bullets ] (items <#> HH.li_ <<< map inline)
    Code _ body -> HH.pre [ css styles.codeBlock ] [ HH.code_ [ HH.text body ] ]
  inline = case _ of
    Text text -> HH.text text
    Bold xs -> HH.strong_ (inline <$> xs)
    Italic xs -> HH.em_ (inline <$> xs)
    InlineCode text -> HH.code [ css styles.inlineCode ] [ HH.text text ]
    Link { text, url }
      | safe url -> HH.a [ css styles.link, HP.href url, HP.target "_blank", HP.rel "noopener noreferrer" ] [ HH.text text ]
      | otherwise -> HH.text text
    Mention name ->
      HH.span [ css $ MessageStyle.mention { ownership, self: name == me } ]
        [ HH.text $ "@" <> name ]
  safe url = Array.any (\scheme -> isJust $ stripPrefix (Pattern scheme) (String.toLower url)) [ "https://", "http://", "mailto:" ]

threaded :: Array Message -> Array (Tuple Continuity Message)
threaded messages = zip ([ Starts ] <> (position <$> zip messages (Array.drop 1 messages))) messages
  where
  position (Tuple previous next)
    | previous.author == next.author && next.sentAt - previous.sentAt < threadWindow && next.replyTo == Nothing = Continues
    | otherwise = Starts

-- | How close two messages must be in time for one to continue the other.
threadWindow :: Number
threadWindow = 300000.0
