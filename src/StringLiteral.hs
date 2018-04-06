{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
-- | Toggle between raw text and string literals.
module StringLiteral where
import qualified Data.List as List
import qualified Data.List.Extra as List.Extra
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import GHC.Stack (HasCallStack)
import qualified System.Environment as Environment
import qualified System.Exit as Exit

import qualified Util


usage :: String
usage =
    "string-literal [ --wrapped --{add,remove,toggle}-{backslash,lines} ]\n\
    \\n\
    \Convert between plain text and either backslash-continued string\n\
    \literals, or list of lines style strings.  This is to work around\n\
    \haskell's lack of multi-line string literals.  Bind the toggle variant\n\
    \to a vim key to switch between raw text and haskell string literal.\n\
    \\n\
    \It assumes a single level of indent for the strings, and leaves the\n\
    \indent even in the raw form, so that the text will continue to fit in\n\
    \more or less the same number of columns.  This means they will look\n\
    \a bit short if printed literally on the terminal, but if you care about\n\
    \that, use --wrapped mode and have some terminal-aware layer do the\n\
    \wrapping.\n\
    \\n\
    \--wrapped mode assumes that someone else will be wrapping the text.\n\
    \It doesn't put in newlines, and separates wrapped with a leading space.\n\
    \A paragraph newline in the input becomes an explicit newline in the\n\
    \haskell string.  Since it assumes someone else is wrapping, it won't\n\
    \preserve your own leading spaces.  If you are doing explicit formatting\n\
    \then don't use --wrapped.\n\
    \\n\
    \Standard CPP doesn't like Haskell string-gap syntax.  You can either use\n\
    \cpphs via -pgmP 'cpphs --cpp', or use lines mode, which is more\n\
    \cluttered but doesn't make CPP mad.  Presumably you have a unlines or\n\
    \Text.unlines call at the front of the list.\n"

main :: IO ()
main = do
    let showUsage = putStr usage >> Exit.exitFailure
    args <- maybe showUsage return . parseArgs =<< Environment.getArgs
    Text.IO.interact $ Text.unlines
        . Util.focusIndented (Util.focusNonEmpty (process args)) . Text.lines

data Operation = Add | Remove deriving (Eq, Show)
data Kind = Backslash | Lines deriving (Eq, Show)

parseArgs :: [String] -> Maybe (Bool, Maybe Operation, Kind)
parseArgs args = add <$> case filter (/="--wrapped") args of
    ["--toggle-backslash"] -> Just (Nothing, Backslash)
    ["--add-backslash"] -> Just (Just Add, Backslash)
    ["--remove-backslash"] -> Just (Just Remove, Backslash)
    ["--toggle-lines"] -> Just (Nothing, Lines)
    ["--add-lines"] -> Just (Just Add, Lines)
    ["--remove-lines"] -> Just (Just Remove, Lines)
    _ -> Nothing
    where
    add (b, c) = (wrapped, b, c)
    wrapped = "--wrapped" `elem` args

process :: (Bool, Maybe Operation, Kind) -> [Text] -> [Text]
process (wrapped, op, kind) = case (wrapped, op, kind) of
    (_, Nothing, Backslash) -> \lines -> if inferBackslashed lines
        then process (wrapped, Just Remove, kind) lines
        else process (wrapped, Just Add, kind) lines
    (_, Nothing, Lines) -> \lines -> if inferList lines
        then process (wrapped, Just Remove, kind) lines
        else process (wrapped, Just Add, kind) lines

    (False, Just Add, Backslash) -> addBackslash
    (False, Just Remove,  Backslash) -> removeBackslash
    (True, Just Add, Backslash) -> addBackslashWrapped
    (True, Just Remove, Backslash) -> removeBackslashWrapped

    (True, Just _, Lines) -> error "--wrapped not supported for lines yet"
    (False, Just Add, Lines) -> addLines
    (False, Just Remove, Lines) -> removeLines

-- * backslashes

inferBackslashed :: [Text] -> Bool
inferBackslashed [] = False
inferBackslashed (line:_) = "\"" `Text.isPrefixOf` Text.stripStart line

{- | Add backslashes assuming the text will be wrapped by someone else later.

    An extra newline becomes a leading \n.  A leading space is added if
    there is no leading \n, except for the first line.  Other than that,
    leading and trailing spaces are stripped, since the assumption is that
    this is just a list of words.

    > this is
    > raw
    >
    > text

    =>

    > "this is\
    > \ raw\
    > \\ntext"
-}
addBackslashWrapped :: [Text] -> [Text]
addBackslashWrapped =
    mapAround start middle end only . collectNewlines . map quote
    . map Text.strip
    where
    start = surround "\"" "\\"
    middle = surround "\\" "\\" . leadingSpace
    end = surround "\\" "\"" . leadingSpace
    only = surround "\"" "\""
    leadingSpace s
        | "\\n" `Text.isPrefixOf` s = s
        | otherwise = " " <> s

collectNewlines :: [Text] -> [Text]
collectNewlines = filter (not . Text.null) . snd . List.mapAccumL collect 0
    where
    collect newlines line
        | Text.strip line == "" = (newlines+1, "")
        | otherwise = (0, Text.replicate newlines "\\n" <> line)

{- | Invert 'addBackslashWrapped'.  Drop a leading space unless there was
    a leading \n.
-}
removeBackslashWrapped :: [Text] -> [Text]
removeBackslashWrapped =
    map unquote
    . map stripLeadingSpace . zipPrev . concatMap addNewlines
    . mapStrip ("\"", "\\") ("\\", "\\") ("\\", "\"") ("\"", "\"")
    where
    addNewlines s =
        replicate (length pre) "" ++ [Text.intercalate "\\n" post]
        where (pre, post) = span Text.null $ Text.splitOn "\\n" s
    stripLeadingSpace (Just "", s) = s
    stripLeadingSpace (_, s)
        | ' ' : c : _ <- Text.unpack s, c /= ' ' = Text.drop 1 s
        | otherwise = s

addBackslash :: [Text] -> [Text]
addBackslash =
    mapSurround ("\"", nl) ("\\", nl) ("\\", "\\n\"") ("\"", "\\n\"")
    . map quote
    where
    nl = "\\n\\"

removeBackslash :: [Text] -> [Text]
removeBackslash = map unquote
    . mapStrip ("\"", nl) ("\\", nl) ("\\", "\\n\"") ("\"", "\\n\"")
    where
    nl = "\\n\\"

-- * lines

inferList :: [Text] -> Bool
inferList [] = False
inferList (line:_) = "[" `Text.isPrefixOf` Text.stripStart line

addLines :: [Text] -> [Text]
addLines =
    concatMap Text.lines -- handle the \n below
    . mapSurround ("[ \"", "\"") (", \"", "\"")
        (", \"", "\"\n]")
        ("[\"", "\"]")
    . map quote

removeLines :: [Text] -> [Text]
removeLines = map unquote
    . mapStrip ("[ \"", "\"") (", \"", "\"") (", \"", "\"") ("[\"", "\"]")
    . dropLast
    where
    -- The last line should be ']'
    dropLast xs = case List.Extra.unsnoc xs of
        Just (xs, x) | x == "]" -> xs
        _ -> xs


-- * util

quote :: Text -> Text
quote = Text.replace "\"" "\\\"" . Text.replace "\\" "\\\\"

unquote :: Text -> Text
unquote = Text.replace "\\\\" "\\" . Text.replace "\\\"" "\""

-- | Apply separate transformations to start, middle, end, or only elements.
-- If there are 2 elements, middle loses out, and if there is 1, the @only@
-- function is applied.
mapAround :: (a -> b) -> (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
mapAround start middle end only xs = case xs of
    [] -> []
    [x] -> [only x]
    x : xs -> start x : go xs
    where
    go xs = case xs of
        [] -> []
        [x] -> [end x]
        x : xs -> middle x : go xs

mapSurround :: (Text, Text) -> (Text, Text) -> (Text, Text) -> (Text, Text)
    -> [Text] -> [Text]
mapSurround start middle end only =
    mapAround (uncurry surround start) (uncurry surround middle)
        (uncurry surround end) (uncurry surround only)

mapStrip :: (Text, Text) -> (Text, Text) -> (Text, Text) -> (Text, Text)
    -> [Text] -> [Text]
mapStrip start middle end only =
    mapAround (uncurry strip start) (uncurry strip middle)
        (uncurry strip end) (uncurry strip only)

surround :: Text -> Text -> Text -> Text
surround s e x = s <> x <> e

strip :: HasCallStack => Text -> Text -> Text -> Text
strip s e = stripPrefix s . stripSuffix e

-- * generic util

-- TODO these are mostly from Util.Seq, I should put that on hackage too

stripSuffix :: HasCallStack => Text -> Text -> Text
stripSuffix s text =
    maybe (error $ "expected suffix " <> show s <> " on " <> show text) id $
        Text.stripSuffix s text

stripPrefix :: HasCallStack => Text -> Text -> Text
stripPrefix s text =
    maybe (error $ "expected prefix " <> show s <> " on " <> show text) id $
        Text.stripPrefix s text

zipPrev :: [a] -> [(Maybe a, a)]
zipPrev xs = zip (Nothing : map Just xs) xs
