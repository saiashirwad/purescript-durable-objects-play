-- | An object's open sockets as a channel for values of type `e`. Obtain one
-- | with `Cloudflare.Durable.sockets`, typed `Sockets (Variant events)`.
-- |
-- | `Sockets` is `Contravariant`: `cmap` narrows the channel to one event,
-- | `cmap (inj (Proxy :: _ "message")) sockets :: Sockets Message`.
module Cloudflare.Durable.Sockets
  ( Sockets
  , broadcast
  , connected
  , fromRaw
  , send
  ) where

import Prelude

import Cloudflare.Durable.Runtime (class MonadRuntime, RawSockets, Socket, liftRuntime, platform)
import Data.Argonaut.Core (Json)
import Data.Functor.Contravariant (class Contravariant)

newtype Sockets e = Sockets { raw :: RawSockets, encode :: e -> Json }

instance contravariantSockets :: Contravariant Sockets where
  cmap f (Sockets s) = Sockets s { encode = s.encode <<< f }

fromRaw :: forall e. (e -> Json) -> RawSockets -> Sockets e
fromRaw encode raw = Sockets { raw, encode }

broadcast :: forall m e. MonadRuntime m => Sockets e -> e -> m Unit
broadcast (Sockets s) e = liftRuntime $ platform "sockets.broadcast" $ s.raw.broadcast (s.encode e)

send :: forall m e. MonadRuntime m => Sockets e -> Socket -> e -> m Unit
send (Sockets s) socket e = liftRuntime $ platform "sockets.send" $ s.raw.send socket (s.encode e)

connected :: forall m e. MonadRuntime m => Sockets e -> m (Array Socket)
connected (Sockets s) = liftRuntime $ platform "sockets.connected" s.raw.connected
