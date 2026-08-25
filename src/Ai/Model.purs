-- | A language model as a capability: one function from a completion to a
-- | reply. Providers build one; `hoist` moves it between monads; `scripted`
-- | answers from a list, for tests.
module Ai.Model
  ( AiError(..)
  , Completion
  , Model(..)
  , Reply
  , Usage
  , complete
  , hoist
  , scripted
  ) where

import Prelude

import Ai.Prompt (Message, Prompt)
import Data.Argonaut.Core (Json)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

type Usage = { prompt :: Int, completion :: Int }

type Completion =
  { prompt :: Prompt
  , tools :: Array Json
  , jsonOnly :: Boolean
  }

type Reply = { message :: Message, finish :: String, usage :: Maybe Usage }

data AiError
  = Transport String
  | BadReply String
  | ToolFailed { name :: String, why :: String }
  | TooManyRounds Int
  | Misconfigured String

derive instance eqAiError :: Eq AiError

instance showAiError :: Show AiError where
  show = case _ of
    Transport why -> "Transport " <> show why
    BadReply why -> "BadReply " <> show why
    ToolFailed r -> "ToolFailed " <> show r
    TooManyRounds n -> "TooManyRounds " <> show n
    Misconfigured why -> "Misconfigured " <> show why

newtype Model m = Model (Completion -> m (Either AiError Reply))

complete :: forall m. Model m -> Completion -> m (Either AiError Reply)
complete (Model run) = run

hoist :: forall m n. (m ~> n) -> Model m -> Model n
hoist nat (Model run) = Model (nat <<< run)

-- | Replies in order; once the script runs out, `BadReply "script exhausted"`.
scripted :: forall m. MonadEffect m => Array Reply -> m (Model m)
scripted replies = do
  remaining <- liftEffect $ Ref.new replies
  pure $ Model \_ -> liftEffect do
    queue <- Ref.read remaining
    case uncons queue of
      Just { head, tail } -> Ref.write tail remaining $> Right head
      Nothing -> pure $ Left $ BadReply "script exhausted"
