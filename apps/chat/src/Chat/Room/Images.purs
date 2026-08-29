module Chat.Room.Images
  ( ImageMime(..)
  , Images
  , attach
  , cleanup
  , exists
  , hooks
  , maxImageChars
  , open
  , parseImageMime
  , printImageMime
  ) where

import Prelude

import Chat.Room.Domain (ImageId(..), mkImageId, printImageId)
import Cloudflare.Durable (Hooks, Runtime, State)
import Cloudflare.Durable as Durable
import Cloudflare.Durable.Alarm as Alarm
import Cloudflare.Durable.Codec (codec)
import Cloudflare.Durable.Sql (Statement)
import Cloudflare.Durable.Sql as Sql
import Cloudflare.Worker as Worker
import Data.Array (any)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.DateTime.Instant (instant, unInstant)
import Data.Divide (divided)
import Data.Int (fromString)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), contains, length, stripPrefix, toLower, trim)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)

foreign import matchesImageMimeImpl :: String -> String -> Boolean

data ImageMime
  = Jpeg
  | Png
  | Webp
  | Gif
  | Avif

derive instance eqImageMime :: Eq ImageMime

newtype Images = Images State

maxImageChars :: Int
maxImageChars = 1800000

abandonedTtlMs :: Number
abandonedTtlMs = 86400000.0

parseImageMime :: String -> Maybe ImageMime
parseImageMime raw = case toLower $ trim raw of
  "image/jpeg" -> Just Jpeg
  "image/png" -> Just Png
  "image/webp" -> Just Webp
  "image/gif" -> Just Gif
  "image/avif" -> Just Avif
  _ -> Nothing

printImageMime :: ImageMime -> String
printImageMime = case _ of
  Jpeg -> "image/jpeg"
  Png -> "image/png"
  Webp -> "image/webp"
  Gif -> "image/gif"
  Avif -> "image/avif"

open :: State -> Runtime Images
open state = do
  Sql.execute state createImages unit
  columns <- Sql.query state imageColumns unit
  unless (any (_ == "uploaded_at") columns) $ Sql.execute state addUploadedAt unit
  unless (any (_ == "attached_at") columns) $ Sql.execute state addAttachedAt unit
  definition <- Sql.one state imageDefinition unit
  unless (contains (Pattern "AUTOINCREMENT") definition) $ Sql.batch state
    [ Sql.command createImagesNext unit
    , Sql.command copyImages unit
    , Sql.command dropImages unit
    , Sql.command renameImages unit
    ]
  pure $ Images state

hooks :: Images -> Hooks
hooks images = Durable.fetchHook (serve images) <> Durable.alarmHook (cleanup images)

exists :: Images -> ImageId -> Runtime Boolean
exists (Images state) id = Sql.first state imageExists id <#> case _ of
  Just _ -> true
  Nothing -> false

attach :: Images -> Number -> Array ImageId -> Runtime Unit
attach (Images state) attachedAt = traverse_ $ Sql.execute state attachImage <<< { id: _, attachedAt }

cleanup :: Images -> Runtime Unit
cleanup images@(Images state) = do
  current <- unwrap <<< unInstant <$> Alarm.now state
  Sql.execute state deleteAbandoned (current - abandonedTtlMs)
  Sql.first state nextAbandoned unit >>= case _ of
    Nothing -> pure unit
    Just uploadedAt -> scheduleEarlier images (uploadedAt + abandonedTtlMs)

serve :: Images -> Worker.Request -> Runtime (Maybe Worker.Response)
serve images@(Images state) request = case Worker.method request, Worker.pathname request of
  "POST", "/image" -> case Worker.header request "content-type" >>= parseImageMime of
    Nothing -> pure $ Just $ Worker.text 415 "send a JPEG, PNG, WebP, GIF, or AVIF image"
    Just mime -> do
      body <- liftAff $ Worker.bodyBase64 request
      if length body > maxImageChars then pure $ Just $ Worker.text 413 "image too large"
      else if not $ matchesImageMimeImpl (printImageMime mime) body then pure $ Just $ Worker.text 415 "image bytes do not match its type"
      else do
        uploadedAt <- unwrap <<< unInstant <$> Alarm.now state
        Sql.first state insertImage { mime: printImageMime mime, data: body, uploadedAt } >>= case _ of
          Nothing -> pure $ Just $ Worker.text 500 "could not store image"
          Just id -> do
            scheduleEarlier images (uploadedAt + abandonedTtlMs)
            pure $ Just $ Worker.json 200 $ CA.encode (CAR.object "Image" { id: codec }) { id }
  "GET", path | Just id <- stripPrefix (Pattern "/image/") path >>= fromString >>= mkImageId ->
    Sql.first state selectImage id <#> case _ of
      Just image -> Just $ Worker.bytes 200 image.mime image.data
      Nothing -> Just $ Worker.text 404 "no such image"
  _, _ -> pure Nothing

scheduleEarlier :: Images -> Number -> Runtime Unit
scheduleEarlier (Images state) millis = case instant (Milliseconds millis) of
  Nothing -> pure unit
  Just candidate -> do
    current <- Alarm.scheduled state
    let
      shouldReplace = case current of
        Nothing -> true
        Just scheduled -> candidate < scheduled
    when shouldReplace $ Alarm.schedule state candidate

createImages :: Statement Unit Unit
createImages = Sql.statement
  "CREATE TABLE IF NOT EXISTS images (id INTEGER PRIMARY KEY AUTOINCREMENT, mime TEXT NOT NULL, data TEXT NOT NULL, uploaded_at REAL NOT NULL, attached_at REAL)"
  Sql.noParams
  (pure unit)

imageColumns :: Statement Unit String
imageColumns = Sql.statement "PRAGMA table_info(images)" Sql.noParams (Sql.columnOf "name")

addUploadedAt :: Statement Unit Unit
addUploadedAt = Sql.statement "ALTER TABLE images ADD COLUMN uploaded_at REAL NOT NULL DEFAULT 0" Sql.noParams (pure unit)

addAttachedAt :: Statement Unit Unit
addAttachedAt = Sql.statement "ALTER TABLE images ADD COLUMN attached_at REAL" Sql.noParams (pure unit)

imageDefinition :: Statement Unit String
imageDefinition = Sql.statement
  "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'images'"
  Sql.noParams
  (Sql.columnOf "sql")

createImagesNext :: Statement Unit Unit
createImagesNext = Sql.statement
  "CREATE TABLE images_next (id INTEGER PRIMARY KEY AUTOINCREMENT, mime TEXT NOT NULL, data TEXT NOT NULL, uploaded_at REAL NOT NULL, attached_at REAL)"
  Sql.noParams
  (pure unit)

copyImages :: Statement Unit Unit
copyImages = Sql.statement
  "INSERT INTO images_next (id, mime, data, uploaded_at, attached_at) SELECT id, mime, data, uploaded_at, attached_at FROM images"
  Sql.noParams
  (pure unit)

dropImages :: Statement Unit Unit
dropImages = Sql.statement "DROP TABLE images" Sql.noParams (pure unit)

renameImages :: Statement Unit Unit
renameImages = Sql.statement "ALTER TABLE images_next RENAME TO images" Sql.noParams (pure unit)

insertImage :: Statement { mime :: String, data :: String, uploadedAt :: Number } ImageId
insertImage = lcmap (\i -> (i.mime /\ i.data) /\ i.uploadedAt) $ Sql.statement
  "INSERT INTO images (mime, data, uploaded_at) VALUES (?, ?, ?) RETURNING id"
  (Sql.param CA.string `divided` Sql.param CA.string `divided` Sql.param CA.number)
  (ImageId <$> Sql.columnOf "id")

selectImage :: Statement ImageId { mime :: String, data :: String }
selectImage = Sql.statement
  "SELECT mime, data FROM images WHERE id = ?"
  Sql.paramOf
  ({ mime: _, data: _ } <$> Sql.columnOf "mime" <*> Sql.columnOf "data")

imageExists :: Statement ImageId Int
imageExists = Sql.statement "SELECT id FROM images WHERE id = ?" Sql.paramOf (Sql.columnOf "id")

attachImage :: Statement { id :: ImageId, attachedAt :: Number } Unit
attachImage = lcmap (\i -> i.attachedAt /\ printImageId i.id) $ Sql.statement
  "UPDATE images SET attached_at = ? WHERE id = ?"
  (Sql.param CA.number `divided` Sql.param CA.int)
  (pure unit)

deleteAbandoned :: Statement Number Unit
deleteAbandoned = Sql.statement
  "DELETE FROM images WHERE attached_at IS NULL AND uploaded_at <= ?"
  (Sql.param CA.number)
  (pure unit)

nextAbandoned :: Statement Unit Number
nextAbandoned = Sql.statement
  "SELECT uploaded_at FROM images WHERE attached_at IS NULL ORDER BY uploaded_at LIMIT 1"
  Sql.noParams
  (Sql.columnOf "uploaded_at")
