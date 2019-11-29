module Main where

import Test.HUnit.Base
import Test.Hspec

import Data
import Eval
import Parse hiding (string)

main :: IO ()
main = hspec spec

-- parse then print then compare
roundTripShouldBe :: String -> String -> Expectation
roundTripShouldBe a b =
  either (expectationFailure . ((a <> ": ") <>) . errorBundlePretty) ((`shouldBe` b) . repr) $
  parse "test case" a

eval :: String -> IO Object
eval s =
  either
    ((fmap undefined) . expectationFailure . ((s <> ": ") <>))
    pure
    (readEval "test case" s)

-- repl then compare
evalShouldBe :: String -> String -> Expectation
evalShouldBe a b = eval a >>= (`shouldBe` b) . repr

spec :: Spec
spec = do

  describe "parsing" do
    let is = roundTripShouldBe
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
      "(\\h \\e \\l \\l \\o)" `is` "\"hello\""
      "\"hello\"" `is` "\"hello\""
    it "parses and prints other examples" do
      "( )" `is` "nil"

  describe "evaluation" do
    let is = evalShouldBe 
    it "evaluates examples from the spec" do
      "t" `is` "t"
      "nil" `is` "nil"
      "o" `is` "o"
      "apply" `is` "apply"

      -- chars should be a list of all characters
      eval "chars" >>= \x ->
        flip maybe (const $ pure ())
          (assertEqual
            "chars should evaluate to" "a list of pairs of (<character> . <binary representation>)"
            (repr x)
          ) $
            properList x >>= traverse \case
              Pair (MkPair (Character _, s)) -> string s
              _ -> Nothing
