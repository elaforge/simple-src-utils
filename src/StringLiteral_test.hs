{-# LANGUAGE FlexibleContexts #-}
import Data.Text (Text)
import qualified GHC.Stack as Stack
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

import qualified StringLiteral


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "tests"
    [ test_backslashWrapped
    , test_backslashWrapped_roundTrip
    , test_backslash
    , test_backslash_roundTrip
    , test_lines
    , test_lines_roundTrip
    ]

run :: Tasty.TestTree -> IO ()
run = Tasty.defaultMain

test_backslashWrapped :: Tasty.TestTree
test_backslashWrapped = Tasty.testGroup "backslashWrapped"
    [ ["    one line"] ==> ["    \"one line\""]
    , ["    line\"with\\junk"] ==> ["    \"line\\\"with\\\\junk\""]
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
test_backslashWrapped_roundTrip =
    Tasty.testGroup "backslashWrapped_roundTrip" $
        map trip roundTripExamplesWrapped
    where
    trip = roundTrip
        StringLiteral.addBackslashWrapped StringLiteral.removeBackslashWrapped

test_backslash :: Tasty.TestTree
test_backslash = Tasty.testGroup "backslash"
    [ ["    one line"] ==> ["    \"one line\\n\""]
    , ["    line\"with\\junk"] ==> ["    \"line\\\"with\\\\junk\\n\""]
    , ["    two", "    lines"] ==> ["    \"two\\n\\", "    \\lines\\n\""]
    ,
        [ "    with an"
        , "    explicit"
        , ""
        , "    newline"
        ] ==>
        [ "    \"with an\\n\\"
        , "    \\explicit\\n\\"
        , "    \\\\n\\"
        , "    \\newline\\n\""
        ]
    ,
        [ "    with"
        , "      explicit"
        , "    indent"
        ] ==>
        [ "    \"with\\n\\"
        , "    \\  explicit\\n\\"
        , "    \\indent\\n\""
        ]
    ]
    where
    (==>) :: Stack.HasCallStack => [Text] -> [Text] -> Tasty.TestTree
    (==>) = test StringLiteral.addBackslash

test_backslash_roundTrip :: Tasty.TestTree
test_backslash_roundTrip =
    Tasty.testGroup "backslash_roundTrip" $ map trip roundTripExamples
    where
    trip = roundTrip StringLiteral.addBackslash StringLiteral.removeBackslash

test_lines :: Tasty.TestTree
test_lines = Tasty.testGroup "lines"
    [ ["    one line"] ==> ["    [\"one line\"]"]
    , ["    two", "    lines"] ==>
        [ "    [ \"two\""
        , "    , \"lines\""
        , "    ]"
        ]
    ,
        [ "    with an"
        , "    explicit"
        , ""
        , "    newline"
        ] ==>
        [ "    [ \"with an\""
        , "    , \"explicit\""
        , "    , \"\""
        , "    , \"newline\""
        , "    ]"
        ]
    ,
        [ "    with"
        , "      explicit"
        , "    indent"
        ] ==>
        [ "    [ \"with\""
        , "    , \"  explicit\""
        , "    , \"indent\""
        , "    ]"
        ]
    ]
    where
    (==>) :: Stack.HasCallStack => [Text] -> [Text] -> Tasty.TestTree
    (==>) = test StringLiteral.addLines

test_lines_roundTrip :: Tasty.TestTree
test_lines_roundTrip =
    Tasty.testGroup "lines_roundTrip" $ map trip roundTripExamples
    where
    trip = roundTrip StringLiteral.addLines StringLiteral.removeLines

roundTripExamplesWrapped :: [[Text]]
roundTripExamplesWrapped =
    [ ["    one line"]
    , ["    two", "    lines"]
    , ["    line\"s", "    with\\junk"]
    ,
        [ "    with an"
        , "    explicit"
        , ""
        , "    newline"
        ]
    ]

roundTripExamples :: [[Text]]
roundTripExamples = roundTripExamplesWrapped ++
    [   [ "    with"
        , "      explicit"
        , "    indent"
        ]
    ]

-- * util

roundTrip :: (Stack.HasCallStack, Show a, Eq a) => (a -> b) -> (b -> a) -> a
    -> Tasty.TestTree
roundTrip f g x = HUnit.testCase (take 70 $ show x) $ g (f x) HUnit.@?= x

test :: (Stack.HasCallStack, Show a, Eq b, Show b) => (a -> b) -> a -> b
    -> Tasty.TestTree
test f x expected = HUnit.testCase (take 70 $ show x) $ f x HUnit.@?= expected
