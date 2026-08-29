module Chat.Style.Session
  ( Styles
  , raw
  , sheet
  , styles
  ) where

import Prelude

import Chat.Style.Foundation (hairline, mutedText)
import UI.Style (Sheet, Style, (:=), create, var)
import UI.Style as Style
import UI.Theme (tokens)

type Styles =
  { screen :: Style
  , card :: Style
  , title :: Style
  , lead :: Style
  , muted :: Style
  , wide :: Style
  }

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
  , lead: mutedText <> create
      [ "margin" := "0"
      , "text-align" := "center"
      , "text-wrap" := "balance"
      ]
  , muted: mutedText <> ("margin" := "0")
  , wide: "inline-size" := "100%"
  }

sheet :: Sheet
sheet = Style.atoms $ Style.sheetFromRecord styles

raw :: String
raw = ""
