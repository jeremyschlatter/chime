module Main where

import Test.Hspec

import Bel

main :: IO ()
main = hspec spec

-- parse then print then compare
is :: String -> String -> Expectation
is a b =
  either (expectationFailure . ((a <> ": ") <>) . errorBundlePretty) ((`shouldBe` b) . repr) $
  parse "test case" a

spec :: Spec
spec = do
  describe "parsing" do
    it "parses and prints spec examples" do
      "foo" `is` "foo"
      "(foo . bar)" `is` "(foo . bar)"
      "(foo . (bar . baz))" `is` "(foo bar . baz)"
      "\\a" `is` "\\a"
      "\\bel" `is` "\\bel"
      "(a . nil)" `is` "(a)"
      "(a . (b . nil))" `is` "(a b)"
      "(a . (b . (c . nil)))" `is` "(a b c)"
      "()" `is` "nil"
      "(a b c)" `is` "(a b c)"
      "(a (b) c)" `is` "(a (b) c)"
      "((a b c))" `is` "((a b c))"
      "(nil)" `is` "(nil)"
      "(a b . c)" `is` "(a b . c)"
    it "parses and prints other examples" do
      "( )" `is` "nil"
