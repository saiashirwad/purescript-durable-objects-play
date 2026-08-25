-- | A language model as a capability: one function from a completion to a
-- | reply. `Ai.Provider` builds one; `hoist` moves it between monads;
-- | `scripted` answers from a list, for tests.
module Ai.Model
  ( AiError(..)
  , Completion
  , Finish(..)
  , Model(..)
  , ModelId(..)
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
import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref

newtype ModelId = ModelId String

derive instance newtypeModelId :: Newtype ModelId _
derive newtype instance eqModelId :: Eq ModelId
derive newtype instance ordModelId :: Ord ModelId

instance showModelId :: Show ModelId where
  show (ModelId id) = "(ModelId " <> show id <> ")"

type Usage = { prompt :: Int, completion :: Int }

type Completion =
  { prompt :: Prompt
  , tools :: Array Json
  , jsonOnly :: Boolean
  }

-- | Why the model stopped, the same across wires.
data Finish = Stop | ToolCalls | Length | Filtered | Other String

derive instance eqFinish :: Eq Finish

instance showFinish :: Show Finish where
  show = case _ of
    Stop -> "Stop"
    ToolCalls -> "ToolCalls"
    Length -> "Length"
    Filtered -> "Filtered"
    Other why -> "(Other " <> show why <> ")"

type Reply = { message :: Message, finish :: Finish, usage :: Maybe Usage }

-- | `Transport` never reached the server; `Rejected` did, and it said no.
data AiError
  = Transport String
  | Rejected { status :: Int, body :: String }
  | BadReply String
  | ToolFailed { name :: String, why :: String }
  | TooManyRounds Int
  | Misconfigured String

derive instance eqAiError :: Eq AiError

instance showAiError :: Show AiError where
  show = case _ of
    Transport why -> "Transport " <> show why
    Rejected r -> "Rejected " <> show r
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
