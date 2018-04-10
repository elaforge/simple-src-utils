{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import qualified GHC.Stack as Stack
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified FmtSignature


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
    [ test_fmt
    ]

run :: Tasty.TestTree -> IO ()
run = Tasty.defaultMain

test_fmt :: Tasty.TestTree
test_fmt = Tasty.testGroup "fmt"
    [ "a -> b" ==> Just "a -> b"
    , "aaa -> bbb -> ccc" ==> Just "aaa -> bbb\n____-> ccc"
    , "aaa -> () -> bbb" ==> Just "aaa -> ()\n____-> bbb"
    , "a -> b -> c -> d" ==> Just "a -> b\n____-> c\n____-> d"
    -- , "a -> b -> c -> d"
    ]
    where
    f = FmtSignature.fmt "____" 12
    (==>) :: Stack.HasCallStack => String -> Maybe String -> Tasty.TestTree
    (==>) = test f

test :: (Stack.HasCallStack, Show a, Eq b, Show b) => (a -> b) -> a -> b
    -> Tasty.TestTree
test f x expected = HUnit.testCase (take 70 $ show x) $ f x HUnit.@?= expected
