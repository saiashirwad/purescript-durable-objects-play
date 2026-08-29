-- | A small Markdown: paragraphs, headings, quotes, bullet lists, fenced
-- | code; bold, italics, inline code, links, bare URLs, and `@mentions`.
-- | Parsed to data, never to HTML, so rendering can't inject markup.
module Markdown
  ( Block(..)
  , Inline(..)
  , inlines
  , mentions
  , validMentionName
  , nodes
  , parse
  , plain
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Array (concatMap, cons, drop, mapMaybe, nub, null, span, takeWhile, uncons)
import Data.Array as Array
import Data.CodePoint.Unicode (isAlphaNum, isSpace)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), codePointFromChar, joinWith, split, stripPrefix, trim)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.String.CodePoints (CodePoint, fromCodePointArray, toCodePointArray)
import Data.Tuple (Tuple(..))

data Block
  = Paragraph (Array Inline)
  | Heading Int (Array Inline)
  | Quote (Array Block)
  | Bullets (Array (Array Inline))
  | Code (Maybe String) String

derive instance eqBlock :: Eq Block

instance showBlock :: Show Block where
  show = case _ of
    Paragraph xs -> "(Paragraph " <> show xs <> ")"
    Heading n xs -> "(Heading " <> show n <> " " <> show xs <> ")"
    Quote bs -> "(Quote " <> show bs <> ")"
    Bullets items -> "(Bullets " <> show items <> ")"
    Code lang body -> "(Code " <> show lang <> " " <> show body <> ")"

data Inline
  = Text String
  | Bold (Array Inline)
  | Italic (Array Inline)
  | InlineCode String
  | Link { text :: String, url :: String }
  | Mention String

derive instance eqInline :: Eq Inline

instance showInline :: Show Inline where
  show = case _ of
    Text s -> "(Text " <> show s <> ")"
    Bold xs -> "(Bold " <> show xs <> ")"
    Italic xs -> "(Italic " <> show xs <> ")"
    InlineCode s -> "(InlineCode " <> show s <> ")"
    Link l -> "(Link " <> show l <> ")"
    Mention n -> "(Mention " <> show n <> ")"

-- | Every inline node in a document, in reading order, nested ones included.
-- | The one traversal that `mentions` and the like are folds over.
nodes :: Array Block -> Array Inline
nodes = concatMap fromBlock
  where
  fromBlock = case _ of
    Paragraph xs -> fromInlines xs
    Heading _ xs -> fromInlines xs
    Quote bs -> nodes bs
    Bullets items -> concatMap fromInlines items
    Code _ _ -> []
  fromInlines = concatMap \x -> cons x case x of
    Bold xs -> fromInlines xs
    Italic xs -> fromInlines xs
    _ -> []

-- | Every `@name` in the text, once each.
mentions :: String -> Array String
mentions = nub <<< mapMaybe mention <<< nodes <<< parse
  where
  mention = case _ of
    Mention n -> Just n
    _ -> Nothing

-- | Whether a whole string can be written as one parsed `@mention`.
validMentionName :: String -> Boolean
validMentionName name = not (null points) && Array.all isNameCodePoint points
  where
  points = toCodePointArray name

isNameCodePoint :: CodePoint -> Boolean
isNameCodePoint x = isAlphaNum x || x == codePointFromChar '_' || x == codePointFromChar '-' || x == codePointFromChar '.'

-- | The text alone, for previews and notifications.
plain :: String -> String
plain = joinWith " " <<< map blockText <<< parse
  where
  blockText = case _ of
    Paragraph xs -> inlineText xs
    Heading _ xs -> inlineText xs
    Quote bs -> joinWith " " (blockText <$> bs)
    Bullets items -> joinWith ", " (inlineText <$> items)
    Code _ body -> body
  inlineText = joinWith "" <<< map case _ of
    Text s -> s
    Bold xs -> inlineText xs
    Italic xs -> inlineText xs
    InlineCode s -> s
    Link l -> l.text
    Mention n -> "@" <> n

-- Blocks ---------------------------------------------------------------------

parse :: String -> Array Block
parse = blocks <<< split (Pattern newline) <<< S.replaceAll (Pattern windowsNewline) (S.Replacement newline)
  where
  newline = CU.singleton '\n'
  windowsNewline = CU.singleton '\r' <> newline

blocks :: Array String -> Array Block
blocks input = Array.reverse $ tailRec step { rest: input, acc: [] }
  where
  step state = case uncons state.rest of
    Nothing -> Done state.acc
    Just { head, tail }
      | trim head == "" -> Loop state { rest = tail }
      | Just fence <- stripPrefix (Pattern "```") head ->
          let
            { init: body, rest } = span (\line -> stripPrefix (Pattern "```") line == Nothing) tail
            lang = if trim fence == "" then Nothing else Just (trim fence)
          in
            Loop { rest: drop 1 rest, acc: cons (Code lang (joinWith newline body)) state.acc }
      | Just level <- heading head -> Loop { rest: tail, acc: cons (Heading level.depth (inlines level.text)) state.acc }
      | isQuote head ->
          let
            { init: quoted, rest } = span isQuote state.rest
          in
            Loop { rest, acc: cons (Quote (blocks (unquote <$> quoted))) state.acc }
      | isBullet head ->
          let
            { init: items, rest } = span isBullet state.rest
          in
            Loop { rest, acc: cons (Bullets (inlines <<< unbullet <$> items)) state.acc }
      | otherwise ->
          let
            { init: para, rest } = span isPlain state.rest
          in
            Loop { rest, acc: cons (Paragraph (inlines (joinWith newline para))) state.acc }
  isQuote = (_ /= Nothing) <<< stripPrefix (Pattern ">")
  unquote line = fromMaybe line $ stripPrefix (Pattern "> ") line <|> stripPrefix (Pattern ">") line
  isBullet line = stripPrefix (Pattern "- ") line /= Nothing || stripPrefix (Pattern "* ") line /= Nothing
  unbullet = S.drop 2
  isPlain line = trim line /= "" && not (isQuote line) && not (isBullet line) && heading line == Nothing && stripPrefix (Pattern "```") line == Nothing
  newline = CU.singleton '\n'

heading :: String -> Maybe { depth :: Int, text :: String }
heading line =
  let
    hashes = Array.length $ takeWhile (_ == codePointFromChar '#') $ toCodePointArray line
    rest = S.drop hashes line
  in
    if hashes >= 1 && hashes <= 3 && stripPrefix (Pattern " ") rest /= Nothing then Just { depth: hashes, text: trim rest }
    else Nothing

-- Inlines --------------------------------------------------------------------

inlines :: String -> Array Inline
inlines = merge <<< go <<< toCodePointArray
  where
  go :: Array CodePoint -> Array Inline
  go input = Array.reverse $ tailRec step { rest: input, acc: [] }
    where
    step state = case uncons state.rest of
      Nothing -> Done state.acc
      Just { head: c, tail }
        | c == cp '`', Just (Tuple body rest) <- upTo [ cp '`' ] tail ->
            Loop { rest, acc: cons (InlineCode (str body)) state.acc }
        | c == cp '*', Just { head: c2, tail: t2 } <- uncons tail, c2 == cp '*', Just (Tuple body rest) <- upTo [ cp '*', cp '*' ] t2 ->
            Loop { rest, acc: cons (Bold (inlines (str body))) state.acc }
        | c == cp '*', Just (Tuple body rest) <- upTo [ cp '*' ] tail, not (null body) ->
            Loop { rest, acc: cons (Italic (inlines (str body))) state.acc }
        | c == cp '_', Just (Tuple body rest) <- upTo [ cp '_' ] tail, not (null body) ->
            Loop { rest, acc: cons (Italic (inlines (str body))) state.acc }
        | c == cp '['
        , Just (Tuple label afterLabel) <- upTo [ cp ']' ] tail
        , Just { head: p, tail: afterParen } <- uncons afterLabel
        , p == cp '('
        , Just (Tuple url rest) <- upTo [ cp ')' ] afterParen ->
            Loop { rest, acc: cons (Link { text: str label, url: str url }) state.acc }
        | c == cp '@', name <- takeWhile isName tail, not (null name) ->
            Loop { rest: drop (Array.length name) tail, acc: cons (Mention (str name)) state.acc }
        | Just (Tuple url rest) <- bareUrl state.rest ->
            Loop { rest, acc: cons (Link { text: str url, url: str url }) state.acc }
        | otherwise -> Loop { rest: tail, acc: cons (Text (str [ c ])) state.acc }

  -- Adjacent text runs become one.
  merge = Array.reverse <<< Array.foldl step []
    where
    step acc (Text text) | Just { head: Text previous, tail } <- uncons acc = cons (Text (previous <> text)) tail
    step acc inline = cons inline acc

  upTo :: Array CodePoint -> Array CodePoint -> Maybe (Tuple (Array CodePoint) (Array CodePoint))
  upTo close input = tailRec search { acc: [], rest: input }
    where
    n = Array.length close
    search state
      | Array.take n state.rest == close = Done $ Just $ Tuple (Array.reverse state.acc) (drop n state.rest)
      | otherwise = case uncons state.rest of
          Just { head, tail } | head /= cp '\n' -> Loop { acc: cons head state.acc, rest: tail }
          _ -> Done Nothing

  bareUrl cps =
    let
      isUrlChar x = not (isSpace x) && not (x `Array.elem` [ cp '<', cp '>', cp ')', cp ']', cp '"' ])
      run = takeWhile isUrlChar cps
      text = str run
    in
      if stripPrefix (Pattern "http://") text /= Nothing || stripPrefix (Pattern "https://") text /= Nothing then
        let
          trimmed = stripTrailing text
        in
          Just (Tuple (toCodePointArray trimmed) (drop (S.length trimmed) cps))
      else Nothing
    where
    stripTrailing t = if CU.takeRight 1 t `Array.elem` [ ".", ",", "!", "?", ":" ] then CU.dropRight 1 t else t

  isName = isNameCodePoint
  cp = codePointFromChar
  str = fromCodePointArray
