module Test.Markdown.Main where

import Prelude

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
    Markdown.parse "# Hi @bob\nsee **this** and `x` at https://a.io/p.\n\n> quoted\n- one\n- two\n```purs\nmain = 1\n```"
      ==
        [ Heading 1 [ Text "Hi ", Mention "bob" ]
        , Paragraph [ Text "see ", Bold [ Text "this" ], Text " and ", InlineCode "x", Text " at ", Link { text: "https://a.io/p", url: "https://a.io/p" }, Text "." ]
        , Quote [ Paragraph [ Text "quoted" ] ]
        , Bullets [ [ Text "one" ], [ Text "two" ] ]
        , Code (Just "purs") "main = 1"
        ]
      && Markdown.mentions "@ann and @ann, cc @bob but not a@b.c" == [ "ann", "bob", "b.c" ]

  log "All markdown tests passed."

check :: String -> Aff Boolean -> Aff Unit
check name run = run >>= case _ of
  true -> log $ "ok: " <> name
  false -> liftEffect $ throw $ "failed: " <> name
