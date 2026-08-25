module Test.Journal
  ( Entry
  , JournalApi
  , journalLive
  ) where

import Prelude

import Cloudflare.Durable (Live, Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (NoError, Rpc, method)
import Cloudflare.Durable.Sql (Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Durable.Storage as Storage
import Data.Codec.Argonaut as CA
import Data.Divide (divided)
import Data.Maybe (fromMaybe)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))

type Entry = { id :: Int, amount :: Int }

type JournalApi =
  ( record :: { account :: String, amount :: Int } -> Rpc NoError Unit
  , balance :: String -> Rpc NoError Int
  , entries :: String -> Rpc NoError (Array Entry)
  , mistype :: String -> Rpc NoError String
  , reset :: Unit -> Rpc NoError Unit
  )

journal :: Object "Journal" JournalApi
journal = Durable.object
  { record: method
  , balance: method
  , entries: method
  , mistype: method
  , reset: method
  }

createTable :: Statement Unit Unit
createTable = Sql.statement
  "CREATE TABLE IF NOT EXISTS entries (id INTEGER PRIMARY KEY, account TEXT NOT NULL, amount INTEGER NOT NULL)"
  Sql.noParams
  (pure unit)

insert :: Statement { account :: String, amount :: Int } Unit
insert = lcmap (\e -> e.account /\ e.amount) $ Sql.statement
  "INSERT INTO entries (account, amount) VALUES (?, ?)"
  (Sql.param CA.string `divided` Sql.param CA.int)
  (pure unit)

total :: Statement String Int
total = Sql.statement
  "SELECT COALESCE(SUM(amount), 0) AS total FROM entries WHERE account = ?"
  (Sql.param CA.string)
  (Sql.column "total" CA.int)

byAccount :: Statement String Entry
byAccount = Sql.statement
  "SELECT id, amount FROM entries WHERE account = ? ORDER BY id"
  Sql.paramOf
  ({ id: _, amount: _ } <$> Sql.columnOf "id" <*> Sql.columnOf "amount")

-- | Reads the sum as a string, on purpose.
mistyped :: Statement String String
mistyped = Sql.statement
  "SELECT COALESCE(SUM(amount), 0) AS total FROM entries WHERE account = ?"
  (Sql.param CA.string)
  (Sql.column "total" CA.string)

journalLive :: Live "Journal" JournalApi
journalLive =
  Durable.implement journal ado
    state <- Durable.state
    in
      do
        Sql.execute state createTable unit
        pure
          { record: Sql.execute state insert
          , balance: \account -> fromMaybe 0 <$> Sql.first state total account
          , entries: Sql.query state byAccount
          , mistype: \account -> fromMaybe "" <$> Sql.first state mistyped account
          , reset: \_ -> do
              Storage.deleteAll state
              Sql.execute state createTable unit
          }
