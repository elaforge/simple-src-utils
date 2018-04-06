{-# LANGUAGE OverloadedStrings #-}
module Util where
import qualified Data.Char as Char
import qualified Data.List.Extra as List.Extra
import qualified Data.Text as Text
import Data.Text (Text)


-- | Pass through leading and trailing blank lines.
focusNonEmpty :: ([Text] -> [Text]) -> [Text] -> [Text]
focusNonEmpty f lines = pre ++ f within ++ post
    where (pre, within, post) = span2 (Text.null . Text.strip) lines

span2 :: (a -> Bool) -> [a] -> ([a], [a], [a])
span2 f xs = (pre, in2, post)
    where
    (pre, in1) = span f xs
    (in2, post) = List.Extra.spanEnd f in1

-- | Operate on indented text as if it weren't indented.
focusIndented :: ([Text] -> [Text]) -> [Text] -> [Text]
focusIndented f lines = map indent . f . map (Text.drop indentation) $ lines
    where
    indent line
        | Text.null line = ""
        | otherwise = Text.replicate indentation " " <> line
    indentation
        | null lines = 0
        | otherwise = minimum $
            map (Text.length . Text.takeWhile Char.isSpace) $
            filter (not . Text.all Char.isSpace) lines
