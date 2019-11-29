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
    (fst $ readRunEval "test case" s newState)

-- repl then compare
evalShouldBe :: String -> String -> Expectation
evalShouldBe a b = eval a >>= (`shouldBe` b) . repr

evalShouldBeLike :: String -> (Object -> Maybe a) -> String -> Expectation
evalShouldBeLike s f desc = eval s >>= \x ->
  maybe (assertEqual (s <> " should evaluate to") desc (repr x)) (const $ pure ()) (f x)

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
      "'a" `is` "(quote a)"
    it "parses and prints other examples" do
      "( )" `is` "nil"

  describe "evaluation" do
    let is = evalShouldBe 
    let isLike = evalShouldBeLike
    it "evaluates examples from the spec" do
      "t" `is` "t"
      "nil" `is` "nil"
      "o" `is` "o"
      "apply" `is` "apply"

      ("chars" `isLike` \x -> properList x >>= traverse \case
          Pair (MkPair (Character _, s)) -> string s
          _ -> Nothing
       ) "a list of pairs of (<character> . <binary representation>)"

      let varValList = \x -> properList x >>= traverse \case
                          Pair (MkPair (Symbol _, _)) -> Just ()
                          _ -> Nothing
      ("globe" `isLike` varValList) "a list of (var . val) pairs"
      ("scope" `isLike` varValList) "a list of (var . val) pairs"

      "ins" `is` "nil"
      "outs" `is` "nil"
      "(quote a)" `is` "a"
      "'a" `is` "a"
      "(id 'a 'a)" `is` "t"
      "(id 'a 'b)" `is` "nil"
