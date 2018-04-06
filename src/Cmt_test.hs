{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified GHC.Stack as Stack
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified Cmt


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
    [ test_toggleCmt
    ]

run :: Tasty.TestTree -> IO ()
run = Tasty.defaultMain

test_toggleCmt :: Tasty.TestTree
test_toggleCmt = Tasty.testGroup "toggleCmt"
    [ ["x"] ==> ["-- x"]
    , ["  x"] ==> ["  -- x"]
    , ["    x", "      y"] ==> ["    -- x", "    --   y"]
    , [" ", "x", "  "] ==> [" ", "-- x", "  "]
    , [" ", "-- x", "  "] ==> [" ", "x", "  "]
    , ["--x"] ==> ["x"]
    , ["x", "-- y"] ==> ["-- x", "-- -- y"]
    ]
    where
    (==>) :: Stack.HasCallStack => [Text] -> [Text] -> Tasty.TestTree
    (==>) = test (Cmt.toggleCmt "--")


test :: (Stack.HasCallStack, Show a, Eq b, Show b) => (a -> b) -> a -> b
    -> Tasty.TestTree
test f x expected = HUnit.testCase (take 70 $ show x) $ f x HUnit.@?= expected

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

t3 = toggleCmt "--" tlines
t4 = toggleCmt "--" ["  --hi", "-- there"]
-}
