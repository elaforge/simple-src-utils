{-# LANGUAGE OverloadedStrings #-}
module FmtSignature where
import Control.Applicative ((<|>))
import qualified Control.Arrow as Arrow
import qualified Data.Char as Char
import qualified Data.List as List
import qualified System.Environment
import qualified System.Exit
import qualified System.IO as IO

import qualified Text.ParserCombinators.ReadP as ReadP
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP ((<++))
import qualified Text.Read as Read


usage :: String
usage = "fmt-signature width"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        [width] | Just width <- Read.readMaybe width ->
            interact $ \s -> maybe s id (fmt indent width s)
        _ -> IO.hPutStrLn IO.stderr usage >> System.Exit.exitFailure

Just t0 = parse "a -> b"
Just t1 = parse "a => (bb->c)"

indent :: String
indent = "    "

{-
forall y z. X y => A -> IO z

(a -> b) -> c

leave lines with comments on them alone.  What about
a -> (b -- hi
-> c)

-}

fmt :: String -> Int -> String -> Maybe String
fmt indent width =
    fmap (List.intercalate "\n" . indented . wrap (width - length indent)
        . toWords)
    . parse
    where
    indented (x : xs) = x : map (indent++) xs
    indented [] = []

toWords :: [Parsed] -> [String]
toWords = map (strip . concat) . splitWith (`elem` ["->", "=>"]) . map unparse

-- | I treat a Parens as a single word and don't wrap it.  In the future
-- I should wrap in there if necessary, but with an extra indent.
-- In fact, this is basically just Util.Format, so I should use that, after
-- I extract it.
wrap :: Int -> [String] -> [String]
wrap width = map unwords . split
    where
    split words
        | null words = []
        | null pre = take 1 post : split (drop 1 post)
        | otherwise = pre : split post
        where (pre, post) = breakAccum (>width) accum (-1) words
    accum w word = w + 1 + length word

-- * parse

-- | Parse type signature into a minimal form needed for typechecking.
--
-- This still seems like way overkill, but it still seemed like the simplest
-- way make sure parens are balanced accurately.
data Parsed = Word !String | Arrow !String | Parens ![Parsed]
    deriving (Show)

unparse :: Parsed -> String
unparse (Word s) = s
unparse (Arrow s) = s
unparse (Parens ps) = "(" ++ concatMap unparse ps ++ ")"

parse :: String -> Maybe [Parsed]
parse = fmap collect . run (ReadP.many tokenP)

-- It's not like ReadP is great, but it works and is in base.
run :: ReadP a -> String -> Maybe a
run p s = case ReadP.readP_to_S (p <* ReadP.eof) s of
    (a, "") : _ -> Just a
    _ -> Nothing

collect :: [Parsed] -> [Parsed]
collect (p:ps) = case p of
    Word c -> Word (concat (c:pre)) : collect post
        where (pre, post) = spanWhile isWord ps
    Parens subs -> Parens (collect subs) : collect ps
    _ -> p : collect ps
    where
    isWord (Word c) = Just c
    isWord _ = Nothing
collect [] = []

tokenP :: ReadP Parsed
tokenP = parensP <++ arrowP <++ wordP

arrowP :: ReadP Parsed
arrowP = Arrow <$> (ReadP.string "->" <|> ReadP.string "=>")

wordP :: ReadP Parsed
wordP = Word . (:[]) <$> (ReadP.satisfy (const True))

parensP :: ReadP Parsed
parensP = ReadP.char '(' *> (Parens <$> ReadP.many tokenP) <* ReadP.char ')'


-- * util

-- More copy paste from my library.  I should put it on hackage.

strip :: String -> String
strip = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

-- | Like 'span', but it can transform the spanned sublist.
spanWhile :: (a -> Maybe b) -> [a] -> ([b], [a])
spanWhile f = go
    where
    go [] = ([], [])
    go (a:as) = case f a of
        Just b -> Arrow.first (b:) (go as)
        Nothing -> ([], a : as)

splitWith :: (a -> Bool) -> [a] -> [[a]]
    -- ^ output is non-null, and the contents are also, except the first one
splitWith f xs = map reverse (go f xs [])
    where
    go _ [] collect = [collect]
    go f (x:xs) collect
        | f x = collect : go f xs [x]
        | otherwise = go f xs (x:collect)

breakAccum :: (state -> Bool) -> (state -> a -> state) -> state -> [a]
    -> ([a], [a])
breakAccum done accum = go
    where
    go _ [] = ([], [])
    go state (x:xs)
        | done state2 = ([], x : xs)
        | otherwise = Arrow.first (x:) (go state2 xs)
        where
        state2 = accum state x
