{-# LANGUAGE FlexibleContexts #-}
import qualified GHC.Stack as Stack
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import Data.Text (Text)

import qualified StringLiteral


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
    [ test_backslashWrapped
    , test_backslashWrapped_roundTrip
    ]

run :: Tasty.TestTree -> IO ()
run = Tasty.defaultMain

test_backslashWrapped :: Tasty.TestTree
test_backslashWrapped = Tasty.testGroup "backslashWrapped"
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
    ,
        [ "    with"
        , "      explicit"
        , "    indent"
        ] ==>
        [ "    \"with\\"
        , "    \\ explicit\\"
        , "    \\ indent\""
        ]
    ]
    where
    (==>) :: Stack.HasCallStack => [Text] -> [Text] -> Tasty.TestTree
    (==>) = test StringLiteral.addBackslashWrapped

-- I think I don't need to test removeBackslashWrapped because add ensures
-- it goes from reasonable input to the right haskell, and round trip will
-- ensure it goes back to the original state.
test_backslashWrapped_roundTrip :: Tasty.TestTree
test_backslashWrapped_roundTrip = Tasty.testGroup "roundTrip"
    [ backslashWrapped ["    one line"]
    , backslashWrapped ["    two", "    lines"]
    , backslashWrapped
        [ "    with an"
        , "    explicit"
        , ""
        , "    newline"
        ]
    ]
    where
    backslashWrapped = trip
        StringLiteral.addBackslashWrapped StringLiteral.removeBackslashWrapped

trip :: (Stack.HasCallStack, Show a, Eq a) => (a -> b) -> (b -> a) -> a
    -> Tasty.TestTree
trip f g x = HUnit.testCase (take 70 $ show x) $ g (f x) HUnit.@?= x

test :: (Stack.HasCallStack, Show a, Eq b, Show b) => (a -> b) -> a -> b
    -> Tasty.TestTree
test f x expected = HUnit.testCase (take 70 $ show x) $ f x HUnit.@?= expected

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


(===) :: (Eq a, Show a) => a -> a -> HUnit.Assertion
(===) = (HUnit.@?=)
