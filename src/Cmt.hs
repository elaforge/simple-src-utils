{-# LANGUAGE OverloadedStrings #-}
-- | Toggle between commented and uncommented code.  Works with line comments
-- only.
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Environment
import qualified System.Exit


type Cmt = Text

usage :: String
usage = "usage: cmt cmt_char"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        [cmt] -> do
            Text.IO.interact (ignoreBlankLines (cmtOrUncmt (Text.pack cmt)))
        _ -> do
            putStrLn usage
            System.Exit.exitFailure

ignoreBlankLines :: ([Text] -> [Text]) -> Text -> Text
ignoreBlankLines f text = Text.unlines $ leading ++ f within ++ trailing
    where
    rawLines = Text.lines text
    (leading, mid) = span blankLine rawLines
    (within, trailing) = spanEnd blankLine mid

cmtOrUncmt :: Cmt -> [Text] -> [Text]
cmtOrUncmt cmt lines
    | shouldCmt cmt lines = cmtLines cmt lines
    | otherwise = uncmtLines cmt lines

shouldCmt :: Cmt -> [Text] -> Bool
shouldCmt cmt = not . all (\line -> isCmted cmt line || blankLine line)

isCmted :: Cmt -> Text -> Bool
isCmted cmt line = cmt `Text.isPrefixOf` Text.dropWhile Char.isSpace line

cmtLines :: Cmt -> [Text] -> [Text]
cmtLines cmt lines = map (lineCmt cmt (indentOf lines)) lines

indentOf :: [Text] -> Text
indentOf = maybe "" id . minimumOn Text.length
    . map (Text.takeWhile Char.isSpace) . filter (not . blankLine)

lineCmt :: Cmt -> Text -> Text -> Text
lineCmt cmt indent line
    | Text.null post = indent <> cmt
    | otherwise = indent <> cmt <> " " <> post
    where
    -- I don't use pre because the line may be blank.
    (_, post) = Text.splitAt (Text.length indent) line

uncmtLines :: Cmt -> [Text] -> [Text]
uncmtLines cmt = map (uncmtLine cmt)

uncmtLine :: Cmt -> Text -> Text
uncmtLine cmt line = let s = pre <> post in if blankLine s then "" else s
    where
    (pre, withCmt) = Text.breakOn cmt line
    post = dropSpace (Text.drop (Text.length cmt) withCmt)
    dropSpace t = if " " `Text.isPrefixOf` t then Text.drop 1 t else t

blankLine :: Text -> Bool
blankLine = Text.all Char.isSpace

-- * util

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd f xs = (reverse post, reverse pre)
    where (pre, post) = span f (reverse xs)

minimumOn :: (Ord k) => (a -> k) -> [a] -> Maybe a
minimumOn _ [] = Nothing
minimumOn key xs = Just (List.foldl1' f xs)
    where f low x = if key x < key low then x else low


-- * tests

{-
test_cmtOrUncmt = do
    print $ ignoreBlankLines (const ["yohoho"]) " \n\n  hi\n \n \n\n"

t0 = lineCmt "--" "  " "  hello"
t1 = lineCmt "--" "  " "    "
t2 = indentOf ["  ho", " hi"]

tlines :: [Text]
tlines = ["  ho", "", "  hi"]

tcmt :: Text
tcmt = "--"

t3 = cmtOrUncmt "--" tlines
t4 = cmtOrUncmt "--" ["  --hi", "-- there"]
-}
