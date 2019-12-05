module Main where

import BasePrelude hiding ((>), (>>), (>>>))
import Control.Monad.Trans.Maybe
import Test.HUnit.Base
import Test.Hspec

import Common
import Data
import Eval
import Parse hiding (string)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "parsing" do

    let is = parseThenPrintShouldBe

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
      "'a" `is` "'a"

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

      ("chars" `isLike` flip properListOf \case
         Pair r -> readRef r >>= \case
           MkPair (Character _, s) -> string s
           _ -> empty
         _ -> empty
       ) "a list of pairs of (<character> . <binary representation>)"

      let varValList = flip properListOf \case
                          Pair r -> readRef r <&> \case
                            MkPair (Symbol _, _) -> Just ()
                            _ -> Nothing
                          _ -> empty
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
      "(sym \"foo\")" `is` "foo"
      "(nom 'foo)" `is` "\"foo\""
      "(lit a)" `is` "(lit a)"
      "car" `is` "(lit prim car)"
      "(apply join '(a b))" `is` "(a . b)"
      "(apply join 'a '(b))" `is` "(a . b)"
      "(dyn x 'z (join x 'b))" `is` "(z . b)"
      "((lit clo ((x . a)) (y) (join x y)) 'b)" `is` "(a . b)"

    it "evaluates other examples" do
      "(nom)" `is` "\"nil\""
      "(if)" `is` "nil"
      "(if 'a)" `is` "a"
      "(if t   'a)" `is` "a"
      "(if nil 'a)" `is` "nil"
      "(if t   'a 'b)" `is` "a"
      "(if nil 'a 'b)" `is` "b"
      "(if t   'a  t   'b)" `is` "a"
      "(if nil 'a  t   'b)" `is` "b"
      "(if nil 'a  nil 'b)" `is` "nil"
      "(if t   'a  t   'b 'c)" `is` "a"
      "(if nil 'a  t   'b 'c)" `is` "b"
      "(if nil 'a  nil 'b 'c)" `is` "c"
      "(where (cdr '(a b c)))" `is` "((a b c) d)" -- based on (where (cdr x)) from the spec
      "(after a 'b)" `is` "b"
      "((lit clo nil (x) (join x 'b)) 'a)" `is` "(a . b)"
      "(mac n p e)" `is` "(lit mac (lit clo nil p e))"

  describe "multi-line repl sessions" do

    let replTest = replTestWith builtinsIO
    let (>>) = replInput
    let (>) = replOutput

    it "implements set" $ replTest $ []
      >> "(set x 'a)"
      > "a"
      >> "x"
      > "a"

    it "implements xar and xdr" $ replTest $ []
      >> "(set x '(a . b))"
      > "(a . b)"
      >> "x"
      > "(a . b)"
      >> "(xar x 'c)"
      > "c"
      >> "x"
      > "(c . b)"
      >> "(xdr x 'd)"
      > "d"
      >> "x"
      > "(c . d)"

    it "implements where" $ replTest $ []
      >> "(set x '(a b c))"
      > "(a b c)"
      >> "(where (cdr x))"
      > "((a b c) d)"

    it "implements dyn" $ replTest $ []
      >> "(set x 'a)"
      > "a"
      >> "(dyn x 'z (join x 'b))"
      > "(z . b)"
      >> "x"
      > "a"

    it "understands backquote" $ replTest $ []
      >> "(set x 'a)"
      > "a"
      >> "`(x ,x y)"
      > "(x a y)"
      -- @incomplete: uncomment when numbers are working
      -- >> `(x ,x y ,(+ 1 2))
      -- > "(x a y 3)"
      >> "(set y '(c d))"
      > "(c d)"
      >> "`(a b ,@y e f)"
      > "(a b c d e f)"

    it "destructures arguments" $ replTest $ []
      >> "(def f (x . y) `((,x . x) (,y . y)))"
      > "..."
      >> "(f 'a 'b 'c)"
      > "((a . x) ((b c) . y))"
      >> "(f 'a)"
      > "((a . x) (nil . y))"
      >> "(f)"
      > "<error>"
      >> "(def f ((x y) z) `((,x . x) (,y . y) (,z . z)))"
      > "..."
      >> "(f '(a (b c)) '(d))"
      > "((a . x) ((b c) . y) ((d) . z))"
      >> "(f '(a) '(d))"
      > "<error>"
      >> "(f '(a b c) '(d))"
      > "<error>"

    it "implements macros" $ replTest $ []
      >> "(mac nilwith (x) `(join nil ,x))"
      > "..."
      >>  "(nilwith 'a)"
      > "(nil . a)"
      >> "(mac fn- (parms expr) `(lit clo nil ,parms ,expr))"
      > "..."
      >> "((fn- (x y) (join x y)) 'a 'b)"
      > "(a . b)"

    it "implements scope" $ replTest $ []
      >> "(def foo (x) scope)"
      > "..."
      >> "(foo 'a)"
      > "((x . a))"
      >> "(foo 'b)"
      > "((x . b))"

  describe "bel-in-bel" do
    -- Interpret bel.bel and check that the functions it defines
    -- work as they are specified to in bellanguage.txt.

    let is = evalInPreludeShouldBe
    let replTest = replTestWith preludeIO
    let (>>) = replInput
    let (>) = replOutput

    it "interprets bel.bel correctly" do
      "(no nil)" `is` "t"
      "(no 'a)" `is` "nil"
      "(atom \\a)" `is` "t"
      "(atom nil)" `is` "t"
      "(atom 'a)" `is` "t"
      "(atom '(a))" `is` "nil"
      "(all atom '(a b))" `is` "t"
      "(all atom nil)" `is` "t"
      "(all atom '(a (b c) d))" `is` "nil"
      "(some atom '((a b) (c d)))" `is` "nil"
      "(some atom '((a b) c (d e)))" `is` "(c (d e))"
      "(reduce join '(a b c))" `is` "(a b . c)"
      "(cons 'a '(b c))" `is` "(a b c)"
      "(join 'a '(b c))" `is` "(a b c)"
      "(cons 'a 'b 'c '(d e f))" `is` "(a b c d e f)"
      "(append '(a b c) '(d e f))" `is` "(a b c d e f)"
      "(append '(a) nil '(b c) '(d e f))" `is` "(a b c d e f)"
      "(snoc '(a b c) 'd 'e)" `is` "(a b c d e)"
      "(list)" `is` "nil"
      "(list 'a)" `is` "(a)"
      "(list 'a 'b)" `is` "(a b)"
      "(map car '((a b) (c d) (e f)))" `is` "(a c e)"
      "(map cons '(a b c) '(1 2 3))" `is` "((a . 1) (b . 2) (c . 3))"
      "(map cons '(a b c) '(1 2))" `is` "((a . 1) (b . 2))"
      replTest $ []
        >> "(def block args (reduce (fn (x y) (list (list 'fn 'x y) x)) args))"
        > "..."
        >> "(block 'e1 'e2 'e3)"
        > "((fn x ((fn x e3) e2)) e1)"

-- ----------------------------------------------------------------------------
--                         parsing test helpers

parseThenPrintShouldBe :: String -> String -> Expectation
parseThenPrintShouldBe a b =
  either
    (expectationFailure . ((a <> ": ") <>) . errorBundlePretty)
    (>>= repr >=> (`shouldBe` b))
    (parse @IO "test case" a)

-- ----------------------------------------------------------------------------
--                           eval test helpers

failure :: String -> IO a
failure = fmap undefined . expectationFailure

evalIn :: String -> EvalState -> IO (Object IORef)
evalIn s state =
   fst <$> readThenRunEval "test case" s state >>=
     either
       (\e -> failure $ s <> ": " <> e)
       pure

evalInShouldBe :: String -> String -> EvalState -> Expectation
evalInShouldBe a b = evalIn a >=> repr >=> assertEqual ("> " <> a) b

eval :: String -> IO (Object IORef)
eval s = builtinsIO >>= evalIn s

evalShouldBe :: String -> String -> Expectation
evalShouldBe a b = builtinsIO >>= evalInShouldBe a b

evalShouldBeLike :: String -> (Object IORef -> MaybeT IO a) -> String -> Expectation
evalShouldBeLike s f desc = eval s >>= \x -> repr x >>= \rep ->
  runMaybeT (f x) >>=
    maybe (assertEqual (s <> " should evaluate to") desc rep) (const $ pure ())

-- ----------------------------------------------------------------------------
--                     multi-line repl test helpers

red :: String -> String
red s = "\ESC[31m" <> s <> "\ESC[0m"

clear :: String -> String
clear s = "\ESC[0m" <> s <> "\ESC[0m"

replTestWith :: IO EvalState -> [(String, String)] -> IO ()
replTestWith initialState = bind initialState . go [] . reverse where
  go :: [String] -> [(String, String)] -> EvalState -> IO ()
  go prev ios state = case ios of
    [] -> pure ()
    (in_, out) : rest -> do
      let prefix = intercalate "\n" (reverse ("> " <> in_ : prev))
      (x, s') <- readThenRunEval ("line " <> show (length prev)) in_ state
      if out == "<error>"
      then either
        (const $ pure ())
        (repr >=> expectationFailure . ("Expected an error but got a value: " <>))
        x
      else either
        (expectationFailure . (clear (prefix <> "\n") <>) . red)
        (if out == "..." then const $ pure () else (repr >=> assertEqual prefix out))
        x
      go (out : "> " <> in_ : prev) rest s'

replInput :: [(String, String)] -> String -> ([(String, String)], String)
replInput = (,)

replOutput :: ([(String, String)], String) -> String -> [(String, String)]
replOutput (ios, i) o = (i, o) : ios

-- ----------------------------------------------------------------------------
--                     bel-in-bel test helpers

preludeIO :: IO EvalState
preludeIO = do
  let input = "test/part-of-bel.bel"
  es <- bel input
  either
    (\e -> failure $ "failed to parse " <> input <> ": " <> e)
    pure
    es

evalInPreludeShouldBe :: String -> String -> Expectation
evalInPreludeShouldBe a b = preludeIO >>= evalInShouldBe a b
