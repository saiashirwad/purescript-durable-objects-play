-- | Typed LLM agents: `Def` says what an agent is, `mount` attaches a model
-- | and tools, `invoke` runs it. See `Ai.Agent`, `Ai.Tool`, `Ai.Schema`.
module Ai
  ( module Agent
  , module Model
  , module Prompt
  , module Tool
  ) where

import Ai.Agent (Agent, Def, invoke, mount, rounds, structured, text) as Agent
import Ai.Model (AiError(..), Model, Reply, hoist, scripted) as Model
import Ai.Prompt (Message(..), Prompt, ToolCall, assistant, system, user) as Prompt
import Ai.Tool (Tool, Toolkit, tool) as Tool
