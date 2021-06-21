{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import qualified GHC.Stack as Stack
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified FmtSignature


main :: IO ()
main = run $ Tasty.testGroup "tests"
    [ test_fmt, test_wrap
    ]

run :: Tasty.TestTree -> IO ()
run = Tasty.defaultMain

test_fmt :: Tasty.TestTree
test_fmt = Tasty.testGroup "fmt"
    [ "a -> b" ==> Just "a -> b"
    , "abc1234567890 -> a" ==> Just "abc1234567890\n    -> a"
    , "aaa -> bbb -> ccc" ==> Just "aaa -> bbb\n    -> ccc"
    , "aaa -> () -> bbb" ==> Just "aaa -> ()\n    -> bbb"
    , "a -> b -> c -> d" ==> Just "a -> b -> c\n    -> d"
    -- ()s don't break
    , "a -> (b -> c) -> d" ==> Just "a\n    -> (b -> c)\n    -> d"
    ]
    where
    (==>) :: Stack.HasCallStack => String -> Maybe String -> Tasty.TestTree
    (==>) = test (FmtSignature.fmt config)

test_wrap :: Tasty.TestTree
test_wrap = Tasty.testGroup "wrap"
    [ ["aaa", "-> bbb", "-> ccc"] ==> ["aaa -> bbb", "    -> ccc"]
    ]
    where
    (==>) :: Stack.HasCallStack => [String] -> [String] -> Tasty.TestTree
    (==>) = test (FmtSignature.wrap config)

config :: FmtSignature.Config
config = FmtSignature.Config 4 12

test :: (Stack.HasCallStack, Show a, Eq b, Show b) => (a -> b) -> a -> b
    -> Tasty.TestTree
test f x expected = HUnit.testCase (take 70 $ show x) $ f x HUnit.@?= expected
