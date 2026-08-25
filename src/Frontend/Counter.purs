-- | A page for poking the Counter. It talks to the object through the same
-- | `Namespace "Counter" CounterApi` the Worker uses, over `/rpc`.
module Frontend.Counter
  ( main
  ) where

import Prelude

import Cloudflare.Durable (Namespace)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Http as Http
import Cloudflare.Durable.Rpc (NoError, Rpc)
import Cloudflare.Durable.Rpc as Rpc
import Data.Array (take, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Example.Counter (CounterApi, counter)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

counters :: Namespace "Counter" CounterApi
counters = Http.connect "/rpc" counter

type Entry = { name :: String, call :: String, outcome :: String }

type State =
  { name :: String
  , value :: Maybe Int
  , busy :: Boolean
  , history :: Array Entry
  }

data Action
  = Initialize
  | SetName String
  | Refresh
  | Increment

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI page unit body

page :: forall query input output m. MonadAff m => H.Component query input output m
page = H.mkComponent
  { initialState: \_ -> { name: "user-123", value: Nothing, busy: false, history: [] }
  , render
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.main_
    [ HH.h1_ [ HH.text "Counter" ]
    , HH.p [ HP.class_ (HH.ClassName "hint") ]
        [ HH.text "One Durable Object per name. The page calls it through "
        , HH.code_ [ HH.text "Namespace \"Counter\" CounterApi" ]
        , HH.text ", the same type the Worker uses."
        ]
    , HH.label_
        [ HH.text "Object name"
        , HH.input
            [ HP.value st.name
            , HP.disabled st.busy
            , HE.onValueInput SetName
            ]
        ]
    , HH.div [ HP.class_ (HH.ClassName "value") ]
        [ HH.text $ maybe "—" show st.value ]
    , HH.div [ HP.class_ (HH.ClassName "actions") ]
        [ HH.button [ HP.disabled st.busy, HE.onClick \_ -> Increment ] [ HH.text "increment" ]
        , HH.button [ HP.disabled st.busy, HE.onClick \_ -> Refresh ] [ HH.text "get" ]
        ]
    , HH.h2_ [ HH.text "Calls" ]
    , HH.ol [ HP.class_ (HH.ClassName "history") ] $ st.history <#> \entry ->
        HH.li_
          [ HH.code_ [ HH.text $ "(getByName counters " <> show entry.name <> ")." <> entry.call <> " unit" ]
          , HH.span_ [ HH.text $ " → " <> entry.outcome ]
          ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> call "get" _.get
  Refresh -> call "get" _.get
  Increment -> call "increment" _.increment
  SetName name -> H.modify_ _ { name = name, value = Nothing }

call
  :: forall output m
   . MonadAff m
  => String
  -> (Record CounterApi -> Unit -> Rpc NoError Int)
  -> H.HalogenM State Action () output m Unit
call label pick = do
  { name } <- H.get
  H.modify_ _ { busy = true }
  result <- liftAff $ Rpc.run $ pick (Durable.getByName counters name) unit
  H.modify_ \st -> st
    { busy = false
    , value = case result of
        Right value -> Just value
        Left _ -> st.value
    , history = take 20 $ { name, call: label, outcome: show result } : st.history
    }
