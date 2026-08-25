-- | A small Markdown: paragraphs, headings, quotes, bullet lists, fenced
-- | code; bold, italics, inline code, links, bare URLs, and `@mentions`.
-- | Parsed to data, never to HTML, so rendering can't inject markup.
module Chat.Markdown
  ( Block(..)
  , Inline(..)
  , inlines
  , mentions
  , parse
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (concatMap, cons, drop, nub, null, snoc, span, takeWhile, uncons)
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

-- | Every `@name` in the text, once each.
mentions :: String -> Array String
mentions = nub <<< concatMap fromBlock <<< parse
  where
  fromBlock = case _ of
    Paragraph xs -> fromInlines xs
    Heading _ xs -> fromInlines xs
    Quote bs -> concatMap fromBlock bs
    Bullets items -> concatMap fromInlines items
    Code _ _ -> []
  fromInlines = concatMap case _ of
    Mention n -> [ n ]
    Bold xs -> fromInlines xs
    Italic xs -> fromInlines xs
    _ -> []

-- Blocks ---------------------------------------------------------------------

parse :: String -> Array Block
parse = blocks <<< split (Pattern "\n") <<< S.replaceAll (Pattern "\r\n") (S.Replacement "\n")

blocks :: Array String -> Array Block
blocks lines = case uncons lines of
  Nothing -> []
  Just { head, tail }
    | trim head == "" -> blocks tail
    | Just fence <- stripPrefix (Pattern "```") head ->
        let
          { init: body, rest } = span (\l -> stripPrefix (Pattern "```") l == Nothing) tail
          lang = if trim fence == "" then Nothing else Just (trim fence)
        in
          cons (Code lang (joinWith "\n" body)) (blocks (drop 1 rest))
    | Just level <- heading head -> cons (Heading level.depth (inlines level.text)) (blocks tail)
    | isQuote head ->
        let { init: quoted, rest } = span isQuote lines
        in cons (Quote (blocks (unquote <$> quoted))) (blocks rest)
    | isBullet head ->
        let { init: items, rest } = span isBullet lines
        in cons (Bullets (inlines <<< unbullet <$> items)) (blocks rest)
    | otherwise ->
        let { init: para, rest } = span isPlain lines
        in cons (Paragraph (inlines (joinWith "\n" para))) (blocks rest)
  where
  isQuote = (_ /= Nothing) <<< stripPrefix (Pattern ">")
  unquote l = fromMaybe l $ stripPrefix (Pattern "> ") l <|> stripPrefix (Pattern ">") l
  isBullet l = stripPrefix (Pattern "- ") l /= Nothing || stripPrefix (Pattern "* ") l /= Nothing
  unbullet = S.drop 2
  isPlain l = trim l /= "" && not (isQuote l) && not (isBullet l) && heading l == Nothing && stripPrefix (Pattern "```") l == Nothing

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
  go cps = case uncons cps of
    Nothing -> []
    Just { head: c, tail }
      | c == cp '`', Just (Tuple body rest) <- upTo [ cp '`' ] tail -> cons (InlineCode (str body)) (go rest)
      | c == cp '*', Just { head: c2, tail: t2 } <- uncons tail, c2 == cp '*', Just (Tuple body rest) <- upTo [ cp '*', cp '*' ] t2 ->
          cons (Bold (inlines (str body))) (go rest)
      | c == cp '*', Just (Tuple body rest) <- upTo [ cp '*' ] tail, not (null body) -> cons (Italic (inlines (str body))) (go rest)
      | c == cp '_', Just (Tuple body rest) <- upTo [ cp '_' ] tail, not (null body) -> cons (Italic (inlines (str body))) (go rest)
      | c == cp '[', Just (Tuple label afterLabel) <- upTo [ cp ']' ] tail
      , Just { head: p, tail: afterParen } <- uncons afterLabel, p == cp '('
      , Just (Tuple url rest) <- upTo [ cp ')' ] afterParen -> cons (Link { text: str label, url: str url }) (go rest)
      | c == cp '@', name <- takeWhile isName tail, not (null name) -> cons (Mention (str name)) (go (drop (Array.length name) tail))
      | Just (Tuple url rest) <- bareUrl cps -> cons (Link { text: str url, url: str url }) (go rest)
      | otherwise -> cons (Text (str [ c ])) (go tail)

  -- Adjacent text runs become one.
  merge = Array.foldr step []
    where
    step (Text a) acc | Just { head: Text b, tail } <- uncons acc = cons (Text (a <> b)) tail
    step x acc = cons x acc

  upTo :: Array CodePoint -> Array CodePoint -> Maybe (Tuple (Array CodePoint) (Array CodePoint))
  upTo close = search []
    where
    n = Array.length close
    search acc rest
      | Array.take n rest == close = Just (Tuple acc (drop n rest))
      | otherwise = case uncons rest of
          Just { head, tail } | head /= cp '\n' -> search (snoc acc head) tail
          _ -> Nothing

  bareUrl cps =
    let
      isUrlChar x = not (isSpace x) && not (x `Array.elem` [ cp '<', cp '>', cp ')', cp ']', cp '"' ])
      run = takeWhile isUrlChar cps
      text = str run
    in
      if stripPrefix (Pattern "http://") text /= Nothing || stripPrefix (Pattern "https://") text /= Nothing then
        let trimmed = stripTrailing text
        in Just (Tuple (toCodePointArray trimmed) (drop (S.length trimmed) cps))
      else Nothing
    where
    stripTrailing t = if CU.takeRight 1 t `Array.elem` [ ".", ",", "!", "?", ":" ] then CU.dropRight 1 t else t

  isName x = isAlphaNum x || x == cp '_' || x == cp '-' || x == cp '.'
  cp = codePointFromChar
  str = fromCodePointArray
