-- | A container-backed object. The image is declared where it is used; the
-- | `containers` block of wrangler config follows from that. Plain HTTP at
-- | `/rpc/Echo/name/<name>/http/...` starts the container on first use and
-- | proxies to it; five idle minutes later the alarm stops it.
module Example.Echo
  ( EchoApi
  , echo
  , echoLive
  ) where

import Prelude

import Cloudflare.Durable (Live, Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Container (Stop(..))
import Cloudflare.Durable.Container as Container
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\))

type EchoApi =
  ( running :: Unit -> Rpc NoError Boolean
  , halt :: Unit -> Rpc NoError Unit
  )

echo :: Object "Echo" EchoApi ()
echo = Durable.object { running: method, halt: method }

port :: Container.Port
port = 8080

echoLive :: Live "Echo" EchoApi ()
echoLive =
  Durable.implementWith echo ado
    state <- Durable.state
    box <- Durable.container (Container.image "./containers/echo/Dockerfile")
    in
      pure $
        Durable.handlers
          { running: \_ -> Container.running box
          , halt: \_ -> Container.stop box Terminate
          }
          `Durable.withHooks`
            ( Durable.fetchHook
                ( \request -> do
                    Container.ensure box port $ Container.env [ "GREETING" /\ "hello from purescript" ]
                    Container.renew state box (Minutes 5.0)
                    Just <$> Container.request box port request
                )
                <> Durable.alarmHook (Container.expire state box)
            )
