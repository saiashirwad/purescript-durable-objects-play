module Test.Markdown.Main where

import Prelude

import Data.Array as Array
import Data.String (joinWith)
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Markdown (Block(..), Inline(..))
import Markdown as Markdown
import Data.Maybe (Maybe(..))

main :: Effect Unit
main = launchAff_ do
  check "markdown parses blocks and inlines, and finds mentions" $ pure $
    Markdown.parse
      """# Hi @bob
see **this** and `x` at https://a.io/p.

> quoted
- one
- two
```purs
main = 1
```"""
      ==
        [ Heading 1 [ Text "Hi ", Mention "bob" ]
        , Paragraph [ Text "see ", Bold [ Text "this" ], Text " and ", InlineCode "x", Text " at ", Link { text: "https://a.io/p", url: "https://a.io/p" }, Text "." ]
        , Quote [ Paragraph [ Text "quoted" ] ]
        , Bullets [ [ Text "one" ], [ Text "two" ] ]
        , Code (Just "purs") "main = 1"
        ]
      && Markdown.mentions "@ann and @ann, cc @bob but not a@b.c" == [ "ann", "bob", "b.c" ]

  check "maximum chat-sized inputs do not exhaust the stack" do
    let long = CodeUnits.fromCharArray $ Array.replicate 4000 'x'
    let unclosed = "*" <> long
    let manyLines = joinWith (CodeUnits.singleton '\n') $ Array.replicate 2000 "x"
    pure $ Markdown.plain long == long
      && Markdown.plain unclosed == unclosed
      && Markdown.plain manyLines == manyLines

  log "All markdown tests passed."

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name
