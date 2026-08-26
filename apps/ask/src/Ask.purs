-- | `spago run -p ask`: one question to DeepSeek, with a tool.
module Ask
  ( main
  ) where

import Prelude

import Ai (invoke, mount, text, tool)
import Ai.Catalogue as Catalogue
import Ai.Provider as Provider
import Ai.Schema as Schema
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Formatter.DateTime (formatDateTime)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (now)

foreign import apiKey :: Effect String

main :: Effect Unit
main = launchAff_ do
  key <- liftEffect apiKey
  let
    clock = tool "clock" "The current date and time, UTC"
      (Schema.object {})
      (Schema.object { iso: Schema.string })
      \_ -> liftEffect do
        instant <- now
        pure { iso: either' $ formatDateTime "YYYY-MM-DDTHH:mm:ssZ" (toDateTime instant) }
    assistant = mount (Provider.model Provider.deepseek key Catalogue.deepseekFlash) [ clock ] $
      text "You are terse. Use the clock tool when asked about time."
  answer <- invoke assistant "What time is it, and what's 17 * 23?"
  log case answer of
    Right reply -> reply
    Left failure -> "failed: " <> show failure
  where
  either' = case _ of
    Left e -> e
    Right v -> v
