-- | A conversation as data. `Prompt` is a monoid: `system "..." <> user "..."`.
module Ai.Prompt
  ( Message(..)
  , Prompt(..)
  , ToolCall
  , assistant
  , messages
  , system
  , toolResult
  , user
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..))

type ToolCall = { id :: String, name :: String, arguments :: Json }

data Message
  = System String
  | User String
  | Assistant { text :: Maybe String, toolCalls :: Array ToolCall }
  | ToolResult { callId :: String, content :: String }

newtype Prompt = Prompt (Array Message)

derive newtype instance semigroupPrompt :: Semigroup Prompt
derive newtype instance monoidPrompt :: Monoid Prompt

messages :: Prompt -> Array Message
messages (Prompt ms) = ms

system :: String -> Prompt
system = Prompt <<< pure <<< System

user :: String -> Prompt
user = Prompt <<< pure <<< User

assistant :: String -> Prompt
assistant text = Prompt [ Assistant { text: Just text, toolCalls: [] } ]

toolResult :: String -> String -> Prompt
toolResult callId content = Prompt [ ToolResult { callId, content } ]
