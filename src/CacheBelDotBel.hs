module CacheBelDotBel where

import Data.FileEmbed
import Data.Text
import Data.Text.Encoding
import Language.Haskell.TH.Syntax

import Common hiding (evaluate)
import Data
import Eval
import Parse

serializedBelDotBelState :: Q Exp
serializedBelDotBelState = runIO (preludeIO >>= stateToString) >>= liftString

stringToState :: (MonadMutableRef m, Ref m ~ IORef, MonadIO m)
  => String -> m (Either String EvalState)
stringToState s = parse "" s >>= \case
  Left err -> pure $ Left $ errorBundlePretty err
  Right obj -> objectToState obj

belDotBel :: String
belDotBel = unpack $ decodeUtf8 $
  $(embedFile "src/prelude.bel") <> $(embedFile "src/extensions.bel")

preludeIO :: IO EvalState
preludeIO = do
  b <- builtinsIO
  prog <- parseMany "bel.bel" belDotBel >>= either (die . errorBundlePretty) pure
  (x, s) <- runEval (traverse_ evaluate prog $> Symbol Nil) b
  either
    (\e -> interpreterBug $ "failed to interpret bel.bel: " <> e)
    pure
    (x $> s)
