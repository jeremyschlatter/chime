module Main where

import BasePrelude
import Control.Monad.Trans.Maybe
import Test.HUnit.Base
import Test.Hspec

import Data
import Eval
import Parse hiding (string)

main :: IO ()
main = hspec spec

--- parse then print then compare
roundTripShouldBe :: String -> String -> Expectation
roundTripShouldBe a b =
  either
    (expectationFailure . ((a <> ": ") <>) . errorBundlePretty)
    (>>= repr >=> (`shouldBe` b))
    (parse @IO "test case" a)

eval :: String -> IO (Object IORef)
eval s =
   fst <$> (builtinsIO >>= readThenRunEval "test case" s) >>=
     either
       (\e -> undefined <$> (expectationFailure $ s <> ": " <> e))
       pure

-- repl then compare
evalShouldBe :: String -> String -> Expectation
evalShouldBe a b = eval a >>= (repr >=> (`shouldBe` b))

evalShouldBeLike :: String -> (Object IORef -> MaybeT IO a) -> String -> Expectation
evalShouldBeLike s f desc = eval s >>= \x -> repr x >>= \rep ->
  runMaybeT (f x) >>=
    maybe (assertEqual (s <> " should evaluate to") desc rep) (const $ pure ())

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

      ("chars" `isLike` \x -> MaybeT (properList x) >>= traverse \case
         Pair r -> readRef r >>= \case
           MkPair (Character _, s) -> string s
           _ -> pure Nothing
         _ -> pure Nothing
       ) "a list of pairs of (<character> . <binary representation>)"

      let varValList = \x -> MaybeT (properList x) >>= traverse \case
                          Pair r -> readRef r <&> \case
                            MkPair (Symbol _, _) -> Just ()
                            _ -> Nothing
                          _ -> pure Nothing
      ("globe" `isLike` varValList) "a list of (var . val) pairs"
      ("scope" `isLike` varValList) "a list of (var . val) pairs"

      "ins" `is` "nil"
      "outs" `is` "nil"
      "(quote a)" `is` "a"
      "'a" `is` "a"
      "(id 'a 'a)" `is` "t"
      "(id 'a 'b)" `is` "nil"
      "(join 'a 'b)" `is` "(a . b)"
      "(join 'a)" `is` "(a)"
      "(id (join 'a 'b) (join 'a 'b))" `is` "nil"
      "(car '(a . b))" `is` "a"
      "(car '(a b))" `is` "a"
      "(cdr '(a . b))" `is` "b"
      "(cdr '(a b))" `is` "(b)"
      "(type 'a)" `is` "symbol"
      "(type '(a))" `is` "pair"
      "(type \\a)" `is` "char"
