{-# LANGUAGE FlexibleContexts #-}
import qualified GHC.Stack as Stack
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import Data.Text (Text)

import qualified StringLiteral


(===) :: (Eq a, Show a) => a -> a -> HUnit.Assertion
(===) = (HUnit.@?=)

run = Tasty.defaultMain

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
    [ test_backslashExplicit
    , test_roundTrip
    ]

test_backslashExplicit :: Tasty.TestTree
test_backslashExplicit = Tasty.testGroup "backslashExplicit"
    [ ["    one line"] ==> ["    \"one line\""]
    , ["    two", "    lines"] ==>
        [ "    \"two\\"
        , "    \\ lines\""
        ]
    ,
        [ "    with an"
        , "    explicit"
        , ""
        , "    newline"
        ] ==>
        [ "    \"with an\\"
        , "    \\ explicit\\"
        , "    \\\\nnewline\""
        ]
    ]
    where
    (==>) :: Stack.HasCallStack => [Text] -> [Text] -> Tasty.TestTree
    (==>) = test StringLiteral.addBackslashExplicit

-- I think I don't need to test removeBackslashExplicit because add ensures
-- it goes from reasonable input to the right haskell, and round trip will
-- ensure it goes back to the original state.
test_roundTrip :: Tasty.TestTree
test_roundTrip = Tasty.testGroup "roundTrip"
    [ backslashExplicit ["    one line"]
    , backslashExplicit ["    two", "    lines"]
    ]
    where
    backslashExplicit = trip
        StringLiteral.addBackslashExplicit StringLiteral.removeBackslashExplicit

trip :: (Stack.HasCallStack, Show a, Eq a) => (a -> b) -> (b -> a) -> a
    -> Tasty.TestTree
trip f g x = HUnit.testCase (take 70 $ show x) $ g (f x) HUnit.@?= x

test :: (Stack.HasCallStack, Show a, Eq b, Show b) => (a -> b) -> a -> b
    -> Tasty.TestTree
test f x expected = HUnit.testCase (take 70 $ show x) $ f x HUnit.@?= expected

-- -- TODO real tests
-- _test_addBackslashExplicit = Text.unlines $ addBackslashExplicit
--     [ "this is"
--     , "raw"
--     , ""
--     , "text"
--     ]
--
-- _test_removeBackslashExplicit = Text.unlines $ removeBackslashExplicit $
--     Text.lines _test_addBackslashExplicit
--
-- _test_addBackslash = Text.IO.putStr $ Text.unlines $ addBackslash
--     [ "    foo"
--     , ""
--     , "    bar"
--     ]

-- test1 = HUnit.testCase "test1"
--     [ [1, 2, 3] `compare` [1,2] === GT
--     , [1, 2, 3] `compare` [1,2,2] === LT
--     ]

-- unitTests :: Tasty.TestTree
-- unitTests = Tasty.testGroup "Unit tests"
--     [ HUnit.testCase "blah" $
--       [1, 2, 3] `compare` [1,2] @?= GT
--
--     -- the following test does not hold
--     , HUnit.testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--     ]

