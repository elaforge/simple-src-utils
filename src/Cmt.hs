{-# LANGUAGE OverloadedStrings #-}
-- | Toggle between commented and uncommented code.  Works with line comments
-- only.
module Cmt (main, toggleCmt) where
import qualified Data.Char as Char
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Environment
import qualified System.Exit

import qualified Util


type Cmt = Text

usage :: String
usage = "usage: cmt cmt_char"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    case args of
        [cmt] -> Text.IO.interact $
            Text.unlines . toggleCmt (Text.pack cmt) . Text.lines
        _ -> do
            putStrLn usage
            System.Exit.exitFailure

toggleCmt :: Cmt -> [Text] -> [Text]
toggleCmt cmt = Util.focusIndented $ Util.focusNonEmpty $ \lines ->
    if shouldCmt cmt lines then map (cmtLine cmt) lines
    else map (uncmtLine cmt) lines

shouldCmt :: Cmt -> [Text] -> Bool
shouldCmt cmt =
    not . all (\line -> cmt `Text.isPrefixOf` line || blankLine line)

cmtLine :: Cmt -> Text -> Text
cmtLine cmt line
    | Text.null line = cmt
    | otherwise = cmt <> " " <> line

uncmtLine :: Cmt -> Text -> Text
uncmtLine cmt line = let s = pre <> post in if blankLine s then "" else s
    where
    (pre, withCmt) = Text.breakOn cmt line
    post = dropSpace (Text.drop (Text.length cmt) withCmt)
    dropSpace t = if " " `Text.isPrefixOf` t then Text.drop 1 t else t

blankLine :: Text -> Bool
blankLine = Text.all Char.isSpace
