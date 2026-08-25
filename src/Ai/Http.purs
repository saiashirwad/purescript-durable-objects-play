-- | The one way an `Ai` provider touches the network. Providers differ only
-- | in the `url` and `headers` they hand this.
module Ai.Http
  ( Header
  , Post
  , post
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Effect (Effect)
import Effect.Aff (Aff)

type Header = { name :: String, value :: String }

type Post = { url :: String, headers :: Array Header, body :: Json } -> Aff { status :: Int, body :: Json }

foreign import postImpl :: { url :: String, headers :: Array Header, body :: Json } -> Effect (Promise { status :: Int, body :: Json })

post :: Post
post = toAffE <<< postImpl
