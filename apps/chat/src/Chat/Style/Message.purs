module Chat.Style.Message
  ( Styles
  , Continuity(..)
  , Ownership(..)
  , actions
  , avatar
  , bubble
  , mention
  , quote
  , quoteText
  , reaction
  , reactions
  , row
  , raw
  , sheet
  , styles
  ) where

import Prelude

import Chat.Style.Foundation (bubbleMax, fastMotion, gutters, hairline, mutedText, pill, smallText, truncate, wash)
import Chat.Style.Hook as Hook
import UI.Style (Sheet, Style, (:=), create, var)
import UI.Style as Style
import UI.Theme (tokens)

data Ownership
  = Own
  | Other

derive instance eqOwnership :: Eq Ownership

data Continuity
  = Starts
  | Continues

derive instance eqContinuity :: Eq Continuity

type Styles =
  { list :: Style
  , message :: Style
  , mine :: Style
  , continued :: Style
  , empty :: Style
  , emptyTitle :: Style
  , muted :: Style
  , gutter :: Style
  , stack :: Style
  , bubble :: Style
  , mineBubble :: Style
  , theirsBubble :: Style
  , mineJoined :: Style
  , theirsJoined :: Style
  , mentionedBubble :: Style
  , author :: Style
  , time :: Style
  , imageLink :: Style
  , image :: Style
  , paragraph :: Style
  , subheading :: Style
  , bullets :: Style
  , blockquote :: Style
  , codeBlock :: Style
  , inlineCode :: Style
  , link :: Style
  , mention :: Style
  , mineMention :: Style
  , selfMention :: Style
  , quote :: Style
  , mineQuote :: Style
  , quoteAuthor :: Style
  , quoteText :: Style
  , mineQuoteText :: Style
  , reactions :: Style
  , mineReactions :: Style
  , reaction :: Style
  , activeReaction :: Style
  , reactionCount :: Style
  , actions :: Style
  , mineActions :: Style
  , actionButton :: Style
  , botAvatar :: Style
  }

styles :: Styles
styles =
  { list: create
      [ "display" := "flex"
      , "flex" := "1"
      , "flex-direction" := "column"
      , "gap" := var tokens.space3
      , "list-style" := "none"
      , "margin" := "0"
      , "padding-block" := "1.5rem"
      , "padding-inline" := gutters
      ]
  , message: create
      [ "align-items" := "flex-end"
      , "animation" := "chat-message-rise 200ms ease-out"
      , "display" := "flex"
      , "gap" := "0.625rem"
      , "max-inline-size" := "min(80%," <> bubbleMax <> ")"
      ]
  , mine: create
      [ "align-self" := "flex-end"
      , "flex-direction" := "row-reverse"
      ]
  , continued: "margin-block-start" := "-0.4rem"
  , empty: create
      [ "align-items" := "center"
      , "align-self" := "center"
      , "flex-direction" := "column"
      , "gap" := var tokens.space1
      , "margin" := "auto"
      , "max-inline-size" := "none"
      , "text-align" := "center"
      ]
  , emptyTitle: create
      [ "font-family" := var tokens.fontDisplay
      , "font-size" := "1.75rem"
      , "font-weight" := "400"
      , "letter-spacing" := "-0.01em"
      , "margin" := "0"
      ]
  , muted: mutedText <> ("margin" := "0")
  , gutter: create
      [ "flex" := "none"
      , "inline-size" := "1.75rem"
      ]
  , stack: create
      [ "display" := "flex"
      , "flex-direction" := "column"
      , "min-inline-size" := "0"
      , "position" := "relative"
      ]
  , bubble: create
      [ "background-color" := var tokens.surface
      , "border" := hairline
      , "border-radius" := "1.125rem"
      , "box-shadow" := var tokens.shadow
      , "display" := "flex"
      , "flex-direction" := "column"
      , "font-size" := "0.9375rem"
      , "gap" := "0.15rem"
      , "line-height" := "1.45"
      , "overflow-wrap" := "anywhere"
      , "padding-block" := "0.5rem"
      , "padding-inline" := "0.8rem"
      ]
  , mineBubble: create
      [ "background-color" := var tokens.accent
      , "border-color" := "transparent"
      , "border-end-end-radius" := var tokens.radiusSm
      , "box-shadow" := "inset 0 1px 0 oklch(100% 0 0 / 14%)," <> var tokens.shadow
      , "color" := var tokens.accentText
      ]
  , theirsBubble: "border-end-start-radius" := var tokens.radiusSm
  , mineJoined: "border-start-end-radius" := var tokens.radiusSm
  , theirsJoined: "border-start-start-radius" := var tokens.radiusSm
  , mentionedBubble: "box-shadow" := "0 0 0 2px color-mix(in oklab," <> var tokens.accent <> " 55%,transparent)," <> var tokens.shadow
  , author: mutedText <> create
      [ "font-size" := "0.75rem"
      , "font-weight" := "600"
      , "letter-spacing" := "0.01em"
      ]
  , time: create
      [ "align-self" := "flex-end"
      , "font-size" := "0.65rem"
      , "font-variant-numeric" := "tabular-nums"
      ]
  , imageLink: create
      [ "display" := "block"
      , "margin-block-start" := "0.4rem"
      ]
  , image: create
      [ "block-size" := "auto"
      , "border-radius" := var tokens.radiusMd
      , "display" := "block"
      , "max-block-size" := "20rem"
      , "max-inline-size" := "min(100%,24rem)"
      ]
  , paragraph: create
      [ "margin" := "0"
      , "white-space" := "pre-wrap"
      ]
  , subheading: create
      [ "font-size" := "1rem"
      , "font-weight" := "600"
      , "margin-block" := "0.3rem 0.1rem"
      ]
  , bullets: create
      [ "list-style" := "disc"
      , "margin-block" := "0.2rem 0"
      , "margin-inline" := "1.1rem 0"
      , "padding" := "0"
      ]
  , blockquote: create
      [ "border-inline-start" := "2px solid currentColor"
      , "margin-block" := "0.3rem"
      , "margin-inline" := "0"
      , "opacity" := "0.8"
      , "padding-inline-start" := "0.6rem"
      ]
  , codeBlock: create
      [ "background-color" := wash
      , "border-radius" := var tokens.radiusSm
      , "font-family" := var tokens.fontMono
      , "font-size" := "0.8rem"
      , "margin-block" := "0.35rem"
      , "overflow-x" := "auto"
      , "padding-block" := "0.5rem"
      , "padding-inline" := "0.65rem"
      ]
  , inlineCode: create
      [ "background-color" := wash
      , "border-radius" := "0.3rem"
      , "font-family" := var tokens.fontMono
      , "font-size" := "0.85em"
      , "padding-block" := "0.05rem"
      , "padding-inline" := "0.3rem"
      ]
  , link: create
      [ "color" := "inherit"
      , "text-decoration" := "underline"
      , "text-underline-offset" := "2px"
      ]
  , mention: create
      [ "color" := var tokens.accent
      , "font-weight" := "600"
      ]
  , mineMention: "color" := "inherit"
  , selfMention: create
      [ "background-color" := "color-mix(in srgb,currentColor 18%,transparent)"
      , "border-radius" := "0.3rem"
      , "padding-inline" := "0.2rem"
      ]
  , quote: create
      [ "align-items" := "flex-start"
      , "background-color" := var tokens.tint
      , "border-radius" := "0.5rem"
      , "display" := "flex"
      , "flex-direction" := "column"
      , "font-size" := smallText
      , "gap" := "0"
      , "inline-size" := "100%"
      , "justify-content" := "flex-start"
      , "line-height" := "1.35"
      , "margin-block-end" := "0.3rem"
      , "min-block-size" := "0"
      , "padding-block" := "0.3rem"
      , "padding-inline" := "0.55rem"
      , "text-align" := "start"
      ]
  , mineQuote: create
      [ "background-color" := "oklch(100% 0 0 / 16%)"
      , "color" := "inherit"
      ]
  , quoteAuthor: create
      [ "font-size" := "0.75rem"
      , "font-weight" := "600"
      ]
  , quoteText: mutedText <> truncate <> ("max-inline-size" := "100%")
  , mineQuoteText: create
      [ "color" := "inherit"
      , "opacity" := "0.85"
      ]
  , reactions: create
      [ "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := "0.3rem"
      , "margin-block-start" := "0.3rem"
      ]
  , mineReactions: "justify-content" := "flex-end"
  , reaction: pill <> create
      [ "background-color" := var tokens.surface
      , "border-color" := var tokens.border
      , "font-size" := smallText
      , "min-block-size" := "1.75rem"
      , "padding-block" := "0.1rem"
      , "padding-inline" := "0.55rem"
      ]
  , activeReaction: create
      [ "background-color" := var tokens.accentSoft
      , "border-color" := var tokens.accent
      , "color" := var tokens.text
      ]
  , reactionCount: mutedText <> create
      [ "font-size" := "0.7rem"
      , "font-variant-numeric" := "tabular-nums"
      ]
  , actions: pill <> create
      [ "background-color" := var tokens.surfaceRaised
      , "border" := hairline
      , "box-shadow" := var tokens.shadow
      , "display" := "inline-flex"
      , "gap" := "0.1rem"
      , "inset-block-start" := "-1.1rem"
      , "inset-inline-end" := "0.5rem"
      , "opacity" := "0"
      , "padding" := "0.15rem"
      , "position" := "absolute"
      , "transition" := "opacity " <> fastMotion <> " ease"
      , "z-index" := "1"
      ]
  , mineActions: create
      [ "inset-inline-end" := "auto"
      , "inset-inline-start" := "0.5rem"
      ]
  , actionButton: create
      [ "font-size" := "0.9rem"
      , "min-block-size" := "1.75rem"
      , "padding-block" := "0"
      , "padding-inline" := "0.4rem"
      ]
  , botAvatar: create
      [ "background" := var tokens.accent
      , "color" := var tokens.accentText
      ]
  }

row :: { ownership :: Ownership, continuity :: Continuity } -> Style
row state =
  styles.message
    <> case state.ownership of
      Own -> styles.mine
      Other -> mempty
    <> case state.continuity of
      Starts -> mempty
      Continues -> styles.continued

bubble :: { ownership :: Ownership, continuity :: Continuity, mentioned :: Boolean } -> Style
bubble state =
  styles.bubble
    <> case state.ownership of
      Own -> styles.mineBubble
      Other -> styles.theirsBubble
    <> case state.ownership, state.continuity of
      Own, Continues -> styles.mineJoined
      Other, Continues -> styles.theirsJoined
      _, Starts -> mempty
    <> case state.mentioned of
      true -> styles.mentionedBubble
      false -> mempty

quote :: Ownership -> Style
quote = case _ of
  Own -> styles.quote <> styles.mineQuote
  Other -> styles.quote

quoteText :: Ownership -> Style
quoteText = case _ of
  Own -> styles.quoteText <> styles.mineQuoteText
  Other -> styles.quoteText

reactions :: Ownership -> Style
reactions = case _ of
  Own -> styles.reactions <> styles.mineReactions
  Other -> styles.reactions

reaction :: Boolean -> Style
reaction = case _ of
  true -> styles.reaction <> styles.activeReaction
  false -> styles.reaction

actions :: Ownership -> Style
actions = case _ of
  Own -> styles.actions <> styles.mineActions
  Other -> styles.actions

mention :: { ownership :: Ownership, self :: Boolean } -> Style
mention state =
  styles.mention
    <> case state.ownership of
      Own -> styles.mineMention
      Other -> mempty
    <> case state.self of
      true -> styles.selfMention
      false -> mempty

avatar :: Boolean -> Style
avatar = case _ of
  true -> styles.botAvatar
  false -> mempty

sheet :: Sheet
sheet = Style.atoms (Style.sheetFromRecord styles) <> Style.global raw

raw :: String
raw =
  "@keyframes chat-message-rise{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}"
    <> hoverActions
    <> "@media (hover:none){"
    <> Hook.selector Hook.MessageActions
    <> "{opacity:1}}"

hoverActions :: String
hoverActions =
  Hook.selector Hook.Message
    <> ":hover "
    <> Hook.selector Hook.MessageActions
    <> ","
    <> Hook.selector Hook.Message
    <> ":focus-within "
    <> Hook.selector Hook.MessageActions
    <> "{opacity:1}"
