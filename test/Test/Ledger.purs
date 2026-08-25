module Test.Ledger
  ( LedgerApi
  , LedgerError(..)
  , ledgerLive
  ) where

import Prelude

import Cloudflare.Durable (Live, Object)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Rpc (NoError, Rpc, fail, method, methodWith)
import Cloudflare.Durable.Storage as Storage
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..), fromMaybe)

data LedgerError = Insufficient { balance :: Int, requested :: Int }

derive instance eqLedgerError :: Eq LedgerError

instance showLedgerError :: Show LedgerError where
  show (Insufficient r) = "Insufficient " <> show r

ledgerErrorCodec :: CA.JsonCodec LedgerError
ledgerErrorCodec =
  CA.prismaticCodec "LedgerError" (Just <<< Insufficient) (\(Insufficient r) -> r)
    $ CAR.object "Insufficient" { balance: CA.int, requested: CA.int }

type LedgerApi =
  ( deposit :: Int -> Rpc NoError Int
  , withdraw :: Int -> Rpc LedgerError Int
  , corrupt :: Unit -> Rpc NoError Unit
  , balance :: Unit -> Rpc NoError Int
  )

ledger :: Object "Ledger" LedgerApi ()
ledger = Durable.object
  { deposit: method
  , withdraw: methodWith { request: CA.int, success: CA.int, error: ledgerErrorCodec }
  , corrupt: method
  , balance: method
  }

balanceKey :: Storage.Key Int
balanceKey = Storage.key "balance"

-- | Same key, wrong codec, on purpose.
poisonKey :: Storage.Key String
poisonKey = Storage.key "balance"

ledgerLive :: Live "Ledger" LedgerApi ()
ledgerLive =
  Durable.implement ledger ado
    state <- Durable.state
    in
      pure
        { deposit: \amount -> do
            current <- fromMaybe 0 <$> Storage.get state balanceKey
            Storage.put state balanceKey (current + amount)
            pure (current + amount)
        , withdraw: \requested -> do
            balance <- fromMaybe 0 <$> Storage.get state balanceKey
            when (requested > balance) $ fail $ Insufficient { balance, requested }
            Storage.put state balanceKey (balance - requested)
            pure (balance - requested)
        , corrupt: \_ -> Storage.put state poisonKey "not a number"
        , balance: \_ -> fromMaybe 0 <$> Storage.get state balanceKey
        }
