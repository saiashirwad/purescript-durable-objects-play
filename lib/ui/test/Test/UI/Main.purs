module Test.UI.Main where

import Prelude

import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import UI.Core (nextEnabled)
import UI.Style ((:=), create, on, render)
import UI.Style as Style
import UI.Theme as Theme

main :: Effect Unit
main = do
  check "the last declaration wins in one condition" do
    let css = render [ create [ "color" := "red", "color" := "blue" ] ]
    pure $ contains (Pattern "color:blue") css && not (contains (Pattern "color:red") css)

  check "different conditions remain independent" do
    let css = render [ create [ "color" := "black", on Style.Hover [ "color" := "blue" ] ] ]
    pure $ contains (Pattern "color:black") css && contains (Pattern ":hover{color:blue}") css

  check "focus-within is a typed element state" do
    let css = render [ on Style.FocusWithin [ "border-color" := "blue" ] ]
    pure $ contains (Pattern ":focus-within{border-color:blue}") css

  check "sheets combine atoms before global rules" do
    let css = Style.renderSheet $ Style.atoms [ "color" := "blue" ] <> Style.global "body{margin:0}"
    pure $ contains (Pattern "color:blue") css && contains (Pattern "body{margin:0}") css

  check "theme CSS contains scoped semantic variables" do
    pure $ contains (Pattern ".ui-theme-light") Theme.render
      && contains (Pattern "--ui-color-background") Theme.render
      && contains (Pattern "prefers-color-scheme:dark") Theme.render

  check "roving focus wraps and skips disabled items" do
    let items = [ { disabled: false }, { disabled: true }, { disabled: false } ]
    pure $ nextEnabled 1 items 0 == 2
      && nextEnabled 1 items 2 == 0
      && nextEnabled (-1) items 0 == 2

check :: String -> Effect Boolean -> Effect Unit
check name test = do
  passed <- test
  if passed then log $ "ok - " <> name
  else throw $ "failed - " <> name
