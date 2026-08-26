-- | What each model can do, as data. `Ai.Provider.model` reads it to refuse
-- | a request before sending it. Grow this the way pi-mono grows
-- | `models.generated.ts`; `unlisted` covers a model not yet here.
module Ai.Catalogue
  ( Cost
  , ModelInfo
  , all
  , deepseekFlash
  , deepseekPro
  , find
  , unlisted
  ) where

import Prelude

import Ai.Model (ModelId(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)

-- | US dollars per million tokens.
type Cost = { input :: Number, output :: Number }

type ModelInfo =
  { id :: ModelId
  , provider :: String
  , tools :: Boolean
  , json :: Boolean
  , vision :: Boolean
  , reasoning :: Boolean
  , context :: Int
  , cost :: Maybe Cost
  }

-- | A model the catalogue does not know, assumed able to do everything.
unlisted :: String -> ModelId -> ModelInfo
unlisted provider id = { id, provider, tools: true, json: true, vision: true, reasoning: true, context: 128000, cost: Nothing }

deepseekFlash :: ModelInfo
deepseekFlash = (unlisted "deepseek" (ModelId "deepseek-v4-flash")) { vision = false, reasoning = false }

deepseekPro :: ModelInfo
deepseekPro = (unlisted "deepseek" (ModelId "deepseek-v4-pro")) { vision = false }

all :: Array ModelInfo
all = [ deepseekFlash, deepseekPro ]

-- | By `provider/id`, or by id alone.
find :: String -> Maybe ModelInfo
find name = Array.find (\m -> name == m.provider <> "/" <> unwrap m.id || name == unwrap m.id) all
