module Chat.Room.Domain
  ( Author(..)
  , ImageId(..)
  , MessageId(..)
  , UserName
  , UserNameError(..)
  , assistantName
  , describeUserNameError
  , isAssistant
  , maxUserNameLength
  , mkAuthor
  , mkImageId
  , mkMessageId
  , mkUserName
  , printAuthor
  , printImageId
  , printMessageId
  , printUserName
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.String (length, toLower, trim)
import Data.Maybe (Maybe(..))
import Markdown as Markdown

newtype MessageId = MessageId Int

derive newtype instance eqMessageId :: Eq MessageId
derive newtype instance ordMessageId :: Ord MessageId
derive newtype instance showMessageId :: Show MessageId

instance hasCodecMessageId :: HasCodec MessageId where
  codec = CA.prismaticCodec "MessageId" mkMessageId printMessageId CA.int

newtype ImageId = ImageId Int

derive newtype instance eqImageId :: Eq ImageId
derive newtype instance ordImageId :: Ord ImageId
derive newtype instance showImageId :: Show ImageId

instance hasCodecImageId :: HasCodec ImageId where
  codec = CA.prismaticCodec "ImageId" mkImageId printImageId CA.int

data Author
  = Human UserName
  | Assistant

derive instance eqAuthor :: Eq Author
derive instance ordAuthor :: Ord Author

instance showAuthor :: Show Author where
  show = printAuthor

instance hasCodecAuthor :: HasCodec Author where
  codec = CA.prismaticCodec "Author" (hush <<< mkAuthor) printAuthor CA.string

newtype UserName = UserName String

derive newtype instance eqUserName :: Eq UserName
derive newtype instance ordUserName :: Ord UserName

data UserNameError
  = UserNameRequired
  | UserNameTooLong
  | UserNameInvalid
  | UserNameReserved

derive instance eqUserNameError :: Eq UserNameError

instance showUserNameError :: Show UserNameError where
  show = case _ of
    UserNameRequired -> "UserNameRequired"
    UserNameTooLong -> "UserNameTooLong"
    UserNameInvalid -> "UserNameInvalid"
    UserNameReserved -> "UserNameReserved"

instance hasCodecUserName :: HasCodec UserName where
  codec = CA.prismaticCodec "UserName" (hush <<< mkUserName) printUserName CA.string

assistantName :: String
assistantName = "ai"

mkMessageId :: Int -> Maybe MessageId
mkMessageId id
  | id > 0 = Just $ MessageId id
  | otherwise = Nothing

printMessageId :: MessageId -> Int
printMessageId (MessageId id) = id

mkImageId :: Int -> Maybe ImageId
mkImageId id
  | id > 0 = Just $ ImageId id
  | otherwise = Nothing

printImageId :: ImageId -> Int
printImageId (ImageId id) = id

mkAuthor :: String -> Either UserNameError Author
mkAuthor raw
  | toLower (trim raw) == assistantName = Right Assistant
  | otherwise = Human <$> mkUserName raw

printAuthor :: Author -> String
printAuthor = case _ of
  Human user -> printUserName user
  Assistant -> assistantName

isAssistant :: Author -> Boolean
isAssistant = case _ of
  Human _ -> false
  Assistant -> true

maxUserNameLength :: Int
maxUserNameLength = 32

mkUserName :: String -> Either UserNameError UserName
mkUserName raw =
  let
    name = trim raw
  in
    if name == "" then Left UserNameRequired
    else if length name > maxUserNameLength then Left UserNameTooLong
    else if not (Markdown.validMentionName name) then Left UserNameInvalid
    else if toLower name == assistantName then Left UserNameReserved
    else Right $ UserName name

printUserName :: UserName -> String
printUserName (UserName name) = name

describeUserNameError :: UserNameError -> String
describeUserNameError = case _ of
  UserNameRequired -> "Enter your name."
  UserNameTooLong -> "Use a name with at most " <> show maxUserNameLength <> " characters."
  UserNameInvalid -> "Use only letters, numbers, underscores, hyphens, or periods."
  UserNameReserved -> "That name is reserved."
