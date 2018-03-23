{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Toggle between raw text and string literals.
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import GHC.Stack (HasCallStack)
import qualified System.Environment as Environment
import qualified System.Exit as Exit


usage :: String
usage =
    "StrLit [ --explicit-nl --{add,remove,toggle}-{backslash,lines} ]\n\
    \\n\
    \Convert between plain text and either backslash-continued string\n\
    \literals, or list of lines style strings.  This is to work around\n\
    \haskell's lack of multi-line string literals.\n"

main :: IO ()
main = do
    let showUsage = putStr usage >> Exit.exitFailure
    args <- maybe showUsage return . parseArgs =<< Environment.getArgs
    text <- Text.IO.getContents
    -- TODO Text.interact?
    Text.IO.putStr $ Text.unlines $ process args $ Text.lines text

data Operation = Add | Remove deriving (Eq, Show)
data Kind = Backslash | Lines deriving (Eq, Show)

parseArgs :: [String] -> Maybe (Bool, Maybe Operation, Kind)
parseArgs args = add <$> case filter (/="--explicit-nl") args of
    ["--toggle-backslash"] -> Just (Nothing, Backslash)
    ["--add-backslash"] -> Just (Just Add, Backslash)
    ["--remove-backslash"] -> Just (Just Remove, Backslash)
    ["--toggle-lines"] -> Just (Nothing, Lines)
    ["--add-lines"] -> Just (Just Add, Lines)
    ["--remove-lines"] -> Just (Just Remove, Lines)
    _ -> Nothing
    where
    add (b, c) = (explicitNl, b, c)
    explicitNl = "--explicit-nl" `elem` args

process :: (Bool, Maybe Operation, Kind) -> [Text] -> [Text]
process (explicitNl, op, kind) = case (explicitNl, op, kind) of
    (_, Nothing, Backslash) -> \lines -> if inferBackslashed lines
        then process (explicitNl, Just Remove, kind) lines
        else process (explicitNl, Just Add, kind) lines
    (_, Nothing, Lines) -> \lines -> if inferList lines
        then process (explicitNl, Just Remove, kind) lines
        else process (explicitNl, Just Add, kind) lines

    (False, Just Add, Backslash) -> addBackslash
    (False, Just Remove,  Backslash) -> removeBackslash
    (True, Just Add, Backslash) -> addBackslashExplicit
    (True, Just Remove, Backslash) -> removeBackslashExplicit
    (_, Just Add, Lines) -> addLines
    (_, Just Remove, Lines) -> removeLines

indentation :: Text
indentation = "    "

-- * backslashes

inferBackslashed :: [Text] -> Bool
inferBackslashed [] = False
inferBackslashed (line:_) = "\"" `Text.isPrefixOf` Text.stripStart line

{- |
    An extra newline becomes a leading \n.  A leading space is added if
    there is no leading \n, except for the first line.

    > this is
    > raw
    >
    > text

    =>

    > "this is\
    > \ raw\
    > \\ntext"
-}
addBackslashExplicit :: [Text] -> [Text]
addBackslashExplicit = indent . map3 add1 addn end . collectNewlines . dedent
    where
    add1 s = "\"" <> s <> "\\"
    addn s = "\\" <> leadingSpace s <> "\\"
    end s = "\\" <> leadingSpace s <> "\""
    leadingSpace s
        | "\\n" `Text.isPrefixOf` s = s
        | otherwise = " " <> s

collectNewlines :: [Text] -> [Text]
collectNewlines = filter (not . Text.null) . snd . List.mapAccumL collect 0
    where
    collect newlines line
        | Text.strip line == "" = (newlines+1, "")
        | otherwise = (0, Text.replicate newlines "\\n" <> line)

-- TODO real tests
_test_addBackslashExplicit = Text.unlines $ addBackslashExplicit
    [ "this is"
    , "raw"
    , ""
    , "text"
    ]

_test_removeBackslashExplicit = Text.unlines $ removeBackslashExplicit $
    Text.lines _test_addBackslashExplicit

_test_addBackslash = Text.IO.putStr $ Text.unlines $ addBackslash
    [ "    foo"
    , ""
    , "    bar"
    ]

{- Invert 'addBackslashExplicit'.  Drop a leading space unless there was
    a leading \n.
-}
removeBackslashExplicit :: [Text] -> [Text]
removeBackslashExplicit =
    indent . map stripLeadingSpace . zipPrev . concatMap addNewlines
        . map3 remove1 removen end . dedent
    where
    addNewlines s =
        replicate (length pre) "" ++ [Text.intercalate "\\n" post]
        where (pre, post) = span Text.null $ Text.splitOn "\\n" s
    remove1 = stripPrefix "\"" . stripSuffix "\\"
    removen = stripPrefix "\\" . stripSuffix "\\"
    end = stripPrefix "\\" . stripSuffix "\""
    stripLeadingSpace (Just "", s) = s
    stripLeadingSpace (_, s)
        | ' ' : c : _ <- Text.unpack s, c /= ' ' = Text.drop 1 s
        | otherwise = s

addBackslash :: [Text] -> [Text]
addBackslash = indent . map3 add1 addn end . map quote . dedent
    where
    nl = "\\n\\"
    add1 s = "\"" <> s <> nl
    addn s = "\\" <> s <> nl
    end s = "\\" <> s <> "\\n\""

removeBackslash :: [Text] -> [Text]
removeBackslash = indent . map unquote . map3 remove1 removen end . dedent
    where
    remove1 = stripPrefix "\"" . spaces . nl
    removen = stripPrefix "\\" . spaces . nl
    end = stripPrefix "\\" . spaces . stripSuffix "\\n\""
    spaces = Text.dropWhile (==' ')
    nl = stripSuffix "\\n\\"

-- * lines

inferList :: [Text] -> Bool
inferList [] = False
inferList (line:_) = "[" `Text.isPrefixOf` Text.stripStart line

addLines :: [Text] -> [Text]
addLines = map3 add1 addn end . map quote . dedent
    where
    add1 line = indentation <> "[ \"" <> line <> "\""
    addn line = indentation <> ", \"" <> line <> "\""
    end line = addn line <> "\n" <> indentation <> "]"

removeLines :: [Text] -> [Text]
removeLines = map unquote . map3 remove1 removen id . dropLast
    where
    remove = Text.dropWhile (==' ') . stripSuffix "\""
    remove1 = stripPrefix "[ \"" . remove
    removen = stripPrefix ", \"" . remove
    -- The last line should be ']'
    dropLast [] = []
    dropLast xs = List.init xs

indent :: [Text] -> [Text]
indent = map (\s -> if Text.null s then "" else indentation <> s)

dedent :: [Text] -> [Text]
dedent lines = map (Text.drop indentation) lines
    where
    indentation
        | null lines = 0
        | otherwise = minimum $
            map (Text.length . Text.takeWhile Char.isSpace) $
            filter (not . Text.all Char.isSpace) lines

quote :: Text -> Text
quote = Text.replace "\"" "\\\""

unquote :: Text -> Text
unquote = Text.replace "\\\"" "\""

-- | Apply separate transformations to initial, middle, and final elements.
-- If there are 2 elements, middle loses out, and if there is 1, both
-- initial and final functions are applied.
-- map3 :: (a -> b) -> (a -> b) -> (a -> Maybe b) -> [a] -> [b]
-- map3 initial middle final xs = case xs of
--     [] -> []
--     [x] -> [initial (final x)]
--     x : xs -> initial x : go xs
--     where

map3 :: (a -> b) -> (a -> b) -> (a -> b) -> [a] -> [b]
map3 _ _ _ [] = []
map3 initial middle end (x:xs) = initial x : go xs
    where
    go [] = []
    go [x] = [end x]
    go (x:xs) = middle x : go xs

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
