module Test.Reminder
  ( ReminderApi
  , reminderLive
  ) where

import Prelude

import Cloudflare.Durable (Live, Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Cloudflare.Durable.Storage as Storage
import Data.Array (fromFoldable)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)

type ReminderApi =
  ( remind :: { after :: Number, note :: String } -> Rpc NoError Unit
  , pending :: Unit -> Rpc NoError Boolean
  , fired :: Unit -> Rpc NoError (Array String)
  , forget :: Unit -> Rpc NoError Unit
  )

reminder :: Object "Reminder" ReminderApi ()
reminder = Durable.object
  { remind: method
  , pending: method
  , fired: method
  , forget: method
  }

noteKey :: Storage.Key String
noteKey = Storage.key "note"

fired :: Storage.Prefix String
fired = Storage.prefix "fired:"

reminderLive :: Live "Reminder" ReminderApi ()
reminderLive =
  Durable.implementWith reminder ado
    state <- Durable.state
    in
      pure
        { methods:
            { remind: \{ after, note } -> do
                Storage.put state noteKey note
                Alarm.scheduleIn state (Milliseconds after)
            , pending: \_ -> isJust <$> Alarm.scheduled state
            , fired: \_ -> fromFoldable <<< Map.values <$> Storage.list state fired
            , forget: \_ -> Storage.deleteAll state
            }
        , connect: mempty
        , disconnect: mempty
        , alarm: do
            note <- Storage.get state noteKey
            for_ note \text -> do
              done <- Storage.list state fired
              Storage.put state (fired `Storage.at` show (Map.size done)) text
              void $ Storage.delete state noteKey
        }
