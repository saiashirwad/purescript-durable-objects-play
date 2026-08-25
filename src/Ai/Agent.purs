-- | Agents in three layers.
-- |
-- | `Def i o` says what an agent is: its system prompt, how an input becomes
-- | a user message, how a reply becomes an output. It names no model. A
-- | `Profunctor`: `dimap` adapts input and output.
-- |
-- | `mount model toolkit def` gives an `Agent m i o`: a Kleisli arrow
-- | `i -> ExceptT AiError m o` that runs the model/tool loop. Agents are a
-- | `Category` (`researcher >>> reviewer` is a workflow), `Strong` (carry a
-- | value past an agent with `first`) and `Choice` (route with `left`).
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

import Ai.Model (AiError(..), Finish(..), Model, complete)
import Ai.Prompt (Message(..), Prompt(..), system, toolResult, user)
import Ai.Schema (Schema)
import Ai.Schema as Schema
import Ai.Tool (Toolkit)
import Ai.Tool as Tool
import Control.Monad.Except (ExceptT(..), except, runExceptT, throwError)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, nub)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), either)
import Data.Foldable (fold)
import Data.Maybe (fromMaybe)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Profunctor.Strong (class Strong)
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

newtype Agent m i o = Agent (Star (ExceptT AiError m) i o)

derive newtype instance profunctorAgent :: Functor m => Profunctor (Agent m)
derive newtype instance strongAgent :: Functor m => Strong (Agent m)
derive newtype instance choiceAgent :: Monad m => Choice (Agent m)
derive newtype instance semigroupoidAgent :: Monad m => Semigroupoid (Agent m)
derive newtype instance categoryAgent :: Monad m => Category (Agent m)

invoke :: forall m i o. Agent m i o -> i -> m (Either AiError o)
invoke (Agent (Star run)) = runExceptT <<< run

-- | Attach a model and tools. Duplicate tool names are rejected here, before
-- | any tokens are spent.
mount :: forall m i o. MonadRec m => Model m -> Toolkit m -> Def i o -> Agent m i o
mount model tools (Def d) = Agent $ Star \input -> do
  when (length names /= length (nub names)) $ throwError $ Misconfigured $ "duplicate tool names: " <> show names
  tailRecM round { transcript: d.system <> d.render input, left: d.rounds }
  where
  names = Tool.name <$> tools
  descriptions = Tool.describe <$> tools

  round { left: 0 } = throwError $ TooManyRounds d.rounds
  round { transcript, left } = do
    reply <- ExceptT $ complete model { prompt: transcript, tools: descriptions, jsonOnly: d.jsonOnly }
    let transcript' = transcript <> Prompt [ reply.message ]
    case reply.finish, reply.message of
      ToolCalls, Assistant { toolCalls } -> do
        results <- for toolCalls \call ->
          ExceptT (Right <$> Tool.call tools call) >>= either
            (\why -> throwError $ ToolFailed { name: call.name, why })
            (pure <<< toolResult call.id <<< J.stringify)
        pure $ Loop { transcript: transcript' <> fold results, left: left - 1 }
      Stop, Assistant { text: answer } -> Done <$> except (lmap BadReply $ d.parse $ fromMaybe "" answer)
      Length, _ -> throwError $ BadReply "reply cut off at the length limit"
      Filtered, _ -> throwError $ BadReply "reply withheld by the content filter"
      Other why, _ -> throwError $ BadReply $ "model stopped: " <> why
      _, _ -> throwError $ BadReply "model replied with a non-assistant message"
