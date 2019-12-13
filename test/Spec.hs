module Main where

import BasePrelude hiding ((>), (>>), (>>>))
import Control.Monad.Trans.Maybe
import RawStringsQQ
import Test.HUnit.Base
import Test.Hspec

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
      "[f _ x]" `is` "(fn (_) (f _ x))"
      "(lit num (+ (t t) (t t t)) (+ () (t)))" `is` "2/3"
      "a.b" `is` "(a b)"
      "a!b" `is` "(a 'b)"

    it "parses and prints other examples" do
      "( )" `is` "nil"
      "1" `is` "1"
      "2/3" `is` "2/3"
      "0.5" `is` "1/2"
      "4-1/2i" `is` "4-1/2i"

  describe "evaluation" do

    let is = evalShouldBe
    let isLike = evalShouldBeLike

    it "evaluates examples from the spec" do
      "t" `is` "t"
      "nil" `is` "nil"
      "o" `is` "o"
      "apply" `is` "apply"

      ("chars" `isLike` flip properListOf \case
         Pair ref -> readPair ref >>= \case
           (Character _, s) -> string s
           _ -> empty
         _ -> empty
       ) "a list of pairs of (<character> . <binary representation>)"

      let varValList = flip properListOf \case
                          Pair ref -> readPair ref <&> \case
                            (Symbol _, _) -> Just ()
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

      "'for|2" `is` "(t for 2)"
      -- @consider: the Bel language guide prints this as "(a (quote b) c)"
      -- I prefer the version here, but is that spec-compliant? Same elsewhere.
      "'a!b.c" `is` "(a 'b c)"
      "'!a" `is` "(upon 'a)"
      "'a:b:c" `is` "(compose a b c)"
      "'x|~f:g!a" `is` "(t x ((compose (compose no f) g) 'a))"

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
      "'(id 2.x 3.x)" `is` "(id (2 x) (3 x))"

  describe "multi-line repl sessions" do

    state <- runIO builtinsIO
    let replTest = replTestWith state
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
      >> "`(x ,x y ,(+ 1 2))"
      > "(x a y 3)"
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

  describe "interpretation of bel.bel" do
    -- Interpret bel.bel and check that the functions it defines
    -- work as they are specified to in bellanguage.txt.

    state <- runIO preludeIO
    let is a b = evalInShouldBe a b state
    let replTest = replTestWith state
    let (>>) = replInput
    let (>) = replOutput

    it "reproduces literal repl examples from The Bel Language guide" do
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
      "(let x 'a (cons x 'b))" `is` "(a . b)"
      "(let (x . y) '(a b c) (list x y))" `is` "(a (b c))"
      replTest $ []
        >> "(def block args (reduce (fn (x y) (list (list 'fn 'x y) x)) args))"
        > "..."
        >> "(block 'e1 'e2 'e3)"
        > "((fn x ((fn x e3) e2)) e1)"
      replTest $ []
        >> "((macro (v) `(set ,v 'a)) x)"
        > "a"
        >> "x"
        > "a"
      "(or 'a (prn 'hello))" `is` "a"
      "(apply or '(nil nil))" `is` "nil"
      "(apply or '(nil a b))" `is` "a"
      "(id '(a b) '(a b))" `is` "nil"
      "(= '(a b) '(a b))" `is` "t"
      "(proper nil)" `is` "t"
      "(proper '(a . b))" `is` "nil"
      "(proper '(a b))" `is` "t"
      "((fn (x (o y)) y) 'a)" `is` "nil"
      "((fn (x (o y 'b)) y) 'a)" `is` "b"
      "(let (x (o (y . z) '(a . b))) '(f) (list x y z))" `is` "(f a b)"
      "(string \"foo\")" `is` "t"
      "(mem 'b '(a b c))" `is` "(b c)"
      "(mem 'e '(a b c))" `is` "nil"
      "(mem \\a \"foobar\")" `is` "\"ar\""
      "(mem 3 '(2 4 6 8) >)" `is` "(4 6 8)"
      replTest $ []
        >> "(def sname (s) (case s + 'plus - 'minus 'unknown))"
        > "..."
        >> "(sname '+)"
        > "plus"
      "(iflet x nil 'foo '(a b c) (car x) 'bar)" `is` "a"
      "(map (fn (x) (aif (cdr x) (car it))) '((a) (b c) (d e f) (g)))" `is` "(nil c e nil)"
      "(find [= (car _) \\a] '(\"pear\" \"apple\" \"grape\"))" `is` "\"apple\""
      "(begins '(a b c d e) '(a b))" `is` "t"
      "(caris '(a b) 'a)" `is` "t"
      "(hug '(a b c d e))" `is` "((a b) (c d) (e))"
      "(hug '(1 2 3 4 5) +)" `is` "(3 7 5)"
      "(with (x 'a  \
      \       y 'b) \
      \  (cons x y))" `is` "(a . b)"
      "(keep odd '(1 2 3 4 5))" `is` "(1 3 5)"
      "(rem \\a \"abracadabra\")" `is` "\"brcdbr\""
      "(rem 4 '(5 3 1 2 4) >=)" `is` "(3 1 2)"
      replTest $ []
        >> "(set x '((a . 1) (b . 2) (c . 3)))"
        > "((a . 1) (b . 2) (c . 3))"
        >> "(get 'a x)"
        > "(a . 1)"
        >> "(get 'z x)"
        > "nil"
        >> "(put 'z 26 x)"
        > "((z . 26) (a . 1) (b . 2) (c . 3))"
        >> "(put 'a 9 x)"
        > "((a . 9) (b . 2) (c . 3))"
      "(rev \"able\")" `is` "\"elba\""
      "(snap '(a b) '(1 2 3 4 5))" `is` "((1 2) (3 4 5))"
      "(udrop '(a b) '(1 2 3 4 5))" `is` "(3 4 5)"
      "(map idfn '(a b c))" `is` "(a b c)"
      "((is 'a) 'a)" `is` "t"
      replTest $ []
        >> "(def foo () snerg)"
        > "..."
        >> "(dyn snerg 'a (foo))"
        > "a"
        >> "(let snerg 'a (foo))"
        > "<error>"
      "(list 'a 'b)" `is` "(a b)"
      replTest $ []
        >> [r| (list 'a (ccc (fn (c)
                               (set cont c)
                               'b)))
           |]
        > "(a b)"
        >> "(cont 'z)"
        > "(a z)"
        >> "(cont 'w)"
        > "(a w)"
      [r| (ccc (fn (c)
                 (dyn abort c
                   (do (abort 'a)
                       (car 'b)))))
      |] `is` "a"
      [r|
      (ccc (fn (c)
             (dyn err (fn (x) (c 'hello))
               (car 'b))))
      |] `is` "hello"
      [r|
      (eif x (car 'a)
             'oops
             x)
      |] `is` "oops"
      [r|
      (eif x (car '(a b))
             'oops
             x)
      |] `is` "a"

      replTest $ []
        >> "(def consa ((t xs pair)) (cons 'a xs))"
        > "..."
        >> "(consa 'z)"
        > "<error>"
        >> "(def foo ((o (t (x . y) [caris _ 'a]) '(a . b))) x)"
        > "..."
        >> "(foo '(b b))"
        > "<error>"
        >> "(foo)"
        > "a"

      "((compose car cdr) '(a b c))" `is` "b"
      "(car:cdr '(a b c))" `is` "b"

    it "implements behavior described in The Bel Language guide" do
      "(fn (x) (cons 'a x))" `is` "(lit clo nil (x) (cons 'a x))"
      "((fn (x) (cons 'a x)) 'b)" `is` "(a . b)"
      "((lit clo nil (y) (fn (x) (cons y x))) 'a)" `is` "(lit clo ((y . a)) (x) (cons y x))"
      "(let y 'a (fn (x) (cons y x)))" `is` "(lit clo ((y . a)) (x) (cons y x))"
      "((let y 'a (fn (x) (cons y x))) 'b)" `is` "(a . b)"
      replTest $ []
        >> "(set x 'a)"
        > "a"
        >> "(or 'b (set x 'c))"
        > "b"
        >> "(and t nil (set x 'd))"
        > "nil"
        >> "x"
        > "a" --   <-- the sets should not have evaluated, so x should still be a
      "(and t t nil t)" `is` "nil"
      "(~atom 'a)" `is` "nil"


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

stackTrace :: EvalState -> String
stackTrace =
  (\s -> if s == "" then "<no trace recorded>" else s)
  . intercalate "\n"
  . _debug

evalIn :: String -> EvalState -> IO (Object IORef)
evalIn s state =
   readThenRunEval "test case" s state >>= \(x, postState) ->
     either
       (\e -> failure $ s <> ": " <> e <> clear ("\n\nTrace:\n" <> stackTrace postState))
       pure
       x

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

clear :: String -> String
clear s = "\ESC[0m" <> s <> "\ESC[0m"

replTestWith :: EvalState -> [(String, String)] -> IO ()
replTestWith initialState = flip (go []) initialState . reverse where
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
replOutput (ios, in_) out = (in_, out) : ios

-- ----------------------------------------------------------------------------
--                     bel-in-bel test helpers

preludeIO :: IO EvalState
preludeIO = do
  let input = "reference/bel.bel"
  es <- bel input
  either
    (\e -> failure $ "failed to parse " <> input <> ": " <> e)
    pure
    es
