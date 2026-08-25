-- | Agents in three layers.
-- |
-- | `Def i o` says what an agent is: its system prompt, how an input becomes
-- | a user message, how a reply becomes an output. It names no model. A
-- | `Profunctor`: `dimap` adapts input and output.
-- |
-- | `mount model toolkit def` gives an `Agent m i o`, a function
-- | `i -> m (Either AiError o)` that runs the model/tool loop. Agents are a
-- | `Category`: `researcher >>> reviewer` is a workflow.
module Ai.Agent
  ( Agent(..)
  , Def
  , invoke
  , mount
  , rounds
  , structured
  , text
  ) where

import Prelude

import Ai.Model (AiError(..), Model, complete)
import Ai.Prompt (Message(..), Prompt(..), system, toolResult, user)
import Ai.Schema (Schema)
import Ai.Schema as Schema
import Ai.Tool (Toolkit)
import Ai.Tool as Tool
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array (nub, length)
import Data.Foldable (fold)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either)
import Data.Maybe (fromMaybe)
import Data.Profunctor (class Profunctor)
import Data.Traversable (for)

newtype Def i o = Def
  { system :: Prompt
  , render :: i -> Prompt
  , parse :: String -> Either String o
  , jsonOnly :: Boolean
  , rounds :: Int
  }

instance profunctorDef :: Profunctor Def where
  dimap f g (Def d) = Def d { render = d.render <<< f, parse = map g <<< d.parse }

-- | An agent that answers in prose.
text :: String -> Def String String
text instructions = Def { system: system instructions, render: user, parse: Right, jsonOnly: false, rounds: 8 }

-- | An agent that answers with a value fitting the schema. The schema is
-- | shown to the model and used to decode its JSON.
structured :: forall o. String -> Schema o -> Def String o
structured instructions schema = Def
  { system: system $ instructions <> "\n\nAnswer with json only, matching this JSON Schema:\n" <> J.stringify (Schema.json schema)
  , render: user
  , parse: \raw -> jsonParser raw >>= CA.decode (Schema.codec schema) >>> lmap CA.printJsonDecodeError
  , jsonOnly: true
  , rounds: 8
  }

-- | A round is one model reply and the tool calls it asked for. Eight by default.
rounds :: forall i o. Int -> Def i o -> Def i o
rounds n (Def d) = Def d { rounds = n }

newtype Agent m i o = Agent (i -> m (Either AiError o))

instance profunctorAgent :: Functor m => Profunctor (Agent m) where
  dimap f g (Agent run) = Agent (map (map g) <<< run <<< f)

instance semigroupoidAgent :: Monad m => Semigroupoid (Agent m) where
  compose (Agent g) (Agent f) = Agent \i -> f i >>= either (pure <<< Left) g

instance categoryAgent :: Monad m => Category (Agent m) where
  identity = Agent (pure <<< Right)

invoke :: forall m i o. Agent m i o -> i -> m (Either AiError o)
invoke (Agent run) = run

-- | Attach a model and tools. Duplicate tool names are rejected here, before
-- | any tokens are spent.
mount :: forall m i o. MonadRec m => Model m -> Toolkit m -> Def i o -> Agent m i o
mount model tools (Def d) =
  if length names /= length (nub names) then Agent \_ -> pure $ Left $ Misconfigured $ "duplicate tool names: " <> show names
  else Agent \input -> runExceptT $ tailRecM round { transcript: d.system <> d.render input, left: d.rounds }
  where
  names = Tool.name <$> tools
  descriptions = Tool.describe <$> tools

  round { left: 0 } = throwError $ TooManyRounds d.rounds
  round { transcript, left } = do
    reply <- ExceptT $ complete model { prompt: transcript, tools: descriptions, jsonOnly: d.jsonOnly }
    let transcript' = transcript <> Prompt [ reply.message ]
    case reply.message of
      Assistant { toolCalls } | toolCalls /= [] -> do
        results <- for toolCalls \call -> do
          outcome <- ExceptT $ Right <$> Tool.call tools call
          case outcome of
            Left why -> throwError $ ToolFailed { name: call.name, why }
            Right value -> pure $ toolResult call.id (J.stringify value)
        pure $ Loop { transcript: transcript' <> fold results, left: left - 1 }
      Assistant { text: answer } -> Done <$> ExceptT (pure $ lmap BadReply $ d.parse $ fromMaybe "" answer)
      _ -> throwError $ BadReply "model replied with a non-assistant message"

