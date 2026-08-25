module Frontend.Chat.Style
  ( Styles
  , styles
  , css
  ) where

import Prelude

import Data.String (joinWith)
import Foreign.Object as Object
import UI.Style (Style, (:=), create, on, var)
import UI.Style as Style
import UI.Theme (tokens)

type Styles =
  { -- Auth screens
    screen :: Style
  , card :: Style
  , title :: Style
  , lead :: Style
  , muted :: Style
  , wide :: Style
  -- Room frame
  , room :: Style
  , header :: Style
  , headerGroup :: Style
  , presence :: Style
  , online :: Style
  , roomName :: Style
  , roomId :: Style
  , count :: Style
  , members :: Style
  , member :: Style
  , overlap :: Style
  , identity :: Style
  -- Message list
  , list :: Style
  , message :: Style
  , mine :: Style
  , continued :: Style
  , empty :: Style
  , emptyTitle :: Style
  , gutter :: Style
  , stack :: Style
  -- Bubble
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
  -- Markdown
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
  -- Quoted reply
  , quote :: Style
  , mineQuote :: Style
  , quoteAuthor :: Style
  , quoteText :: Style
  , mineQuoteText :: Style
  -- Reactions and the hover bar
  , reactions :: Style
  , mineReactions :: Style
  , reaction :: Style
  , activeReaction :: Style
  , reactionCount :: Style
  , actions :: Style
  , mineActions :: Style
  , actionButton :: Style
  -- Composer
  , footer :: Style
  , typing :: Style
  , typingVisible :: Style
  , dots :: Style
  , dot :: Style
  , replyChip :: Style
  , replyChipText :: Style
  , attachments :: Style
  , attachment :: Style
  , thumbnail :: Style
  , remove :: Style
  , uploading :: Style
  , suggestions :: Style
  , suggestion :: Style
  , compactAvatar :: Style
  , composer :: Style
  , input :: Style
  , send :: Style
  , largeIcon :: Style
  , botAvatar :: Style
  }

-- | Every style the chat draws with. One record, so the sheet cannot miss
-- | one: a homogeneous record is a `Foreign.Object`, and `values` lists them.
styles :: Styles
styles =
  { screen: create
      [ "background-image" := "radial-gradient(48rem 30rem at 50% -10rem," <> var tokens.accentSoft <> ",transparent 70%)"
      , "display" := "grid"
      , "min-block-size" := "100dvh"
      , "padding" := "1.5rem"
      , "place-items" := "center"
      ]
  , card: create
      [ "background-color" := var tokens.surface
      , "border" := hairline
      , "border-radius" := "1.5rem"
      , "box-shadow" := var tokens.shadow
      , "display" := "grid"
      , "gap" := var tokens.space4
      , "inline-size" := "min(24rem,100%)"
      , "padding-block" := "2.5rem 2rem"
      , "padding-inline" := "2rem"
      ]
  , title: create
      [ "font-family" := var tokens.fontDisplay
      , "font-size" := "2.5rem"
      , "font-weight" := "400"
      , "letter-spacing" := "-0.02em"
      , "line-height" := "1"
      , "margin" := "0"
      , "text-align" := "center"
      ]
  , lead: create
      [ "color" := var tokens.textMuted
      , "margin" := "0"
      , "text-align" := "center"
      , "text-wrap" := "balance"
      ]
  , muted: create
      [ "color" := var tokens.textMuted
      , "margin" := "0"
      ]
  , wide: "inline-size" := "100%"

  , room: create
      [ "block-size" := "100dvh"
      , "display" := "flex"
      , "flex-direction" := "column"
      , "overflow-y" := "auto"
      , "overscroll-behavior" := "contain"
      , "scroll-behavior" := "smooth"
      ]
  , header: glass <> create
      [ "align-items" := "center"
      , "border-block-end" := hairline
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space3
      , "inset-block-start" := "0"
      , "justify-content" := "space-between"
      , "padding-block" := "0.625rem"
      , "padding-inline" := gutters
      , "position" := "sticky"
      , "z-index" := "2"
      ]
  , headerGroup: create
      [ "align-items" := "center"
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space2
      , "min-inline-size" := "0"
      ]
  , presence: create
      [ "background-color" := var tokens.textMuted
      , "block-size" := "0.5rem"
      , "border-radius" := "50%"
      , "inline-size" := "0.5rem"
      , "opacity" := "0.45"
      , "transition" := "background-color 200ms ease,box-shadow 200ms ease,opacity 200ms ease"
      ]
  , online: create
      [ "background-color" := var tokens.success
      , "box-shadow" := "0 0 0 3px color-mix(in oklab," <> var tokens.success <> " 25%,transparent)"
      , "opacity" := "1"
      ]
  , roomName: create
      [ "font-size" := "1rem"
      , "font-weight" := "600"
      , "letter-spacing" := "-0.01em"
      , "margin" := "0"
      ]
  , roomId: create
      [ "background-color" := var tokens.tint
      , "border-radius" := var tokens.radiusSm
      , "color" := var tokens.textMuted
      , "font-family" := var tokens.fontMono
      , "font-size" := "0.75rem"
      , "padding-block" := "0.15rem"
      , "padding-inline" := "0.4rem"
      ]
  , count: create
      [ "color" := var tokens.textMuted
      , "font-size" := "0.8125rem"
      ]
  , members: create
      [ "align-items" := "center"
      , "display" := "flex"
      ]
  , member: create
      [ "block-size" := "1.5rem"
      , "box-shadow" := "0 0 0 2px " <> var tokens.surface
      , "font-size" := "0.6rem"
      , "inline-size" := "1.5rem"
      ]
  , overlap: "margin-inline-start" := "-0.4rem"
  , identity: create
      [ "border-radius" := "999px"
      , "padding-inline" := "0.25rem 0.75rem"
      ]

  , list: create
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
      , "animation" := "rise 200ms ease-out"
      , "display" := "flex"
      , "gap" := "0.625rem"
      , "max-inline-size" := "min(80%,42rem)"
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
  , author: create
      [ "color" := var tokens.textMuted
      , "font-size" := "0.75rem"
      , "font-weight" := "600"
      , "letter-spacing" := "0.01em"
      ]
  , time: create
      [ "align-self" := "flex-end"
      , "font-size" := "0.65rem"
      , "font-variant-numeric" := "tabular-nums"
      , "opacity" := "0.5"
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
      , "font-size" := "0.8125rem"
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
  , quoteText: create
      [ "color" := var tokens.textMuted
      , "max-inline-size" := "100%"
      , "overflow" := "hidden"
      , "text-overflow" := "ellipsis"
      , "white-space" := "nowrap"
      ]
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
  , reaction: create
      [ "background-color" := var tokens.surface
      , "border-color" := var tokens.border
      , "border-radius" := "999px"
      , "font-size" := "0.8125rem"
      , "min-block-size" := "1.75rem"
      , "padding-block" := "0.1rem"
      , "padding-inline" := "0.55rem"
      ]
  , activeReaction: create
      [ "background-color" := var tokens.accentSoft
      , "border-color" := var tokens.accent
      , "color" := var tokens.text
      ]
  , reactionCount: create
      [ "color" := var tokens.textMuted
      , "font-size" := "0.7rem"
      , "font-variant-numeric" := "tabular-nums"
      ]
  , actions: create
      [ "background-color" := var tokens.surfaceRaised
      , "border" := hairline
      , "border-radius" := "999px"
      , "box-shadow" := var tokens.shadow
      , "display" := "inline-flex"
      , "gap" := "0.1rem"
      , "inset-block-start" := "-1.1rem"
      , "inset-inline-end" := "0.5rem"
      , "opacity" := "0"
      , "padding" := "0.15rem"
      , "position" := "absolute"
      , "transition" := "opacity 120ms ease"
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

  , footer: glass <> create
      [ "border-block-start" := hairline
      , "display" := "grid"
      , "gap" := var tokens.space2
      , "inset-block-end" := "0"
      , "padding-block" := "0.5rem calc(0.75rem + env(safe-area-inset-bottom))"
      , "padding-inline" := gutters
      , "position" := "sticky"
      , "z-index" := "2"
      ]
  , typing: create
      [ "align-items" := "center"
      , "color" := var tokens.textMuted
      , "display" := "flex"
      , "font-size" := "0.8125rem"
      , "gap" := var tokens.space2
      , "min-block-size" := "1.25rem"
      , "visibility" := "hidden"
      ]
  , typingVisible: "visibility" := "visible"
  , dots: create
      [ "display" := "inline-flex"
      , "gap" := "0.2rem"
      ]
  , dot: create
      [ "animation" := "blink 1.2s infinite ease-in-out"
      , "background-color" := var tokens.textMuted
      , "block-size" := "0.35rem"
      , "border-radius" := "50%"
      , "inline-size" := "0.35rem"
      ]
  , replyChip: create
      [ "align-items" := "center"
      , "background-color" := var tokens.tint
      , "border-radius" := var tokens.radiusSm
      , "color" := var tokens.textMuted
      , "display" := "flex"
      , "font-size" := "0.8125rem"
      , "gap" := var tokens.space2
      , "padding-block" := "0.3rem"
      , "padding-inline" := "0.6rem"
      ]
  , replyChipText: create
      [ "flex" := "1"
      , "min-inline-size" := "0"
      , "overflow" := "hidden"
      , "text-overflow" := "ellipsis"
      , "white-space" := "nowrap"
      ]
  , attachments: create
      [ "align-items" := "center"
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space3
      ]
  , attachment: create
      [ "display" := "inline-flex"
      , "position" := "relative"
      ]
  , thumbnail: create
      [ "block-size" := "3.5rem"
      , "border" := hairline
      , "border-radius" := var tokens.radiusSm
      ]
  , remove: create
      [ "block-size" := "1.5rem"
      , "box-shadow" := var tokens.shadow
      , "inline-size" := "1.5rem"
      , "inset-block-start" := "-0.5rem"
      , "inset-inline-end" := "-0.5rem"
      , "min-block-size" := "1.5rem"
      , "padding-block" := "0"
      , "padding-inline" := "0"
      , "position" := "absolute"
      ]
  , uploading: create
      [ "color" := var tokens.textMuted
      , "font-size" := "0.8125rem"
      ]
  , suggestions: create
      [ "align-items" := "center"
      , "display" := "flex"
      , "flex-wrap" := "wrap"
      , "gap" := var tokens.space2
      ]
  , suggestion: create
      [ "background-color" := var tokens.surface
      , "border-color" := var tokens.border
      , "border-radius" := "999px"
      , "gap" := "0.4rem"
      , "padding-inline" := "0.3rem 0.75rem"
      ]
  , compactAvatar: create
      [ "block-size" := "1.3rem"
      , "font-size" := "0.6rem"
      , "inline-size" := "1.3rem"
      ]
  , composer: create
      [ "align-items" := "center"
      , "background-color" := var tokens.surface
      , "border" := hairline
      , "border-radius" := "1.5rem"
      , "box-shadow" := var tokens.shadow
      , "display" := "grid"
      , "gap" := var tokens.space1
      , "grid-template-columns" := "auto minmax(0,1fr) auto"
      , "padding-block" := "0.3rem"
      , "padding-inline" := "0.4rem 0.3rem"
      , "transition" := "border-color 120ms ease,box-shadow 120ms ease"
      ]
  , input: create
      [ "background-color" := "transparent"
      , "border-color" := "transparent"
      , "min-block-size" := "2.5rem"
      , "min-inline-size" := "0"
      , "padding-inline" := "0.5rem"
      , on Style.FocusVisible
          [ "border-color" := "transparent"
          , "box-shadow" := "none"
          , "outline" := "none"
          ]
      ]
  , send: create
      [ "block-size" := "2.5rem"
      , "border-radius" := "999px"
      , "inline-size" := "2.5rem"
      , "min-block-size" := "2.5rem"
      , "padding-block" := "0"
      , "padding-inline" := "0"
      ]
  , largeIcon: create
      [ "block-size" := "1.2rem"
      , "inline-size" := "1.2rem"
      ]
  , botAvatar: create
      [ "background" := var tokens.accent
      , "color" := var tokens.accentText
      ]
  }

hairline :: String
hairline = "1px solid " <> var tokens.border

-- | A faint wash of the text color; reads on both bubble colors.
wash :: String
wash = "color-mix(in srgb,currentColor 9%,transparent)"

-- | Side padding that grows to center the content at 72rem.
gutters :: String
gutters = "max(clamp(1rem,3vw,2.5rem),calc((100% - 72rem) / 2))"

-- | Frosted: what scrolls under shows through, blurred.
glass :: Style
glass = create
  [ "-webkit-backdrop-filter" := "blur(14px) saturate(1.4)"
  , "backdrop-filter" := "blur(14px) saturate(1.4)"
  , "background-color" := "color-mix(in oklab," <> var tokens.surface <> " 78%,transparent)"
  ]

css :: String
css = joinWith "\n"
  [ Style.render $ Object.values $ Object.fromHomogeneous styles
  , "@keyframes rise{from{opacity:0;transform:translateY(6px)}to{opacity:1;transform:none}}"
  , "@keyframes blink{0%,80%,100%{opacity:.25;transform:translateY(0)}40%{opacity:1;transform:translateY(-2px)}}"
  , "[data-ui=dots] i:nth-child(2){animation-delay:.2s}[data-ui=dots] i:nth-child(3){animation-delay:.4s}"
  , "[data-ui=message]:hover [data-ui=actions],[data-ui=message]:focus-within [data-ui=actions]{opacity:1}"
  , "[data-ui=composer]:focus-within{border-color:" <> var tokens.accent <> ";box-shadow:0 0 0 3px " <> var tokens.accentSoft <> "," <> var tokens.shadow <> "}"
  ]
