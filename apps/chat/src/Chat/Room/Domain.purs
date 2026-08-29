module Chat.Room.Domain
  ( UserName
  , UserNameError(..)
  , assistantName
  , describeUserNameError
  , maxUserNameLength
  , mkUserName
  , printUserName
  ) where

import Prelude

import Cloudflare.Durable.Codec (class HasCodec)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.String (length, toLower, trim)
import Markdown as Markdown

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
