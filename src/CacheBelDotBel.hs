module CacheBelDotBel where

import BasePrelude
import Control.Monad.Trans.Maybe
import Data.Bitraversable
import Data.FileEmbed
import Language.Haskell.TH.Syntax

import Data
import Eval
import Parse

serializedBelDotBelState :: Q Exp
serializedBelDotBelState = runIO (preludeIO >>= stateToString) >>= liftString

belDotBel :: String
belDotBel = $(embedStringFile "reference/bel.bel")

envToObject :: (MonadRef m, IORef ~ Ref m) => Environment -> m (Object IORef)
envToObject = pureListToObject . fmap Pair

toKVPair :: MonadMutableRef m => Object (Ref m) -> m (Maybe (String, Object (Ref m)))
toKVPair = \case
  Pair ref -> readPair "toKVPair" ref <&> \case
    (Symbol (MkSymbol (toList -> s)), x) -> Just (s, x)
    _ -> Nothing
  _ -> pure Nothing

objectToEnv :: (MonadMutableRef m, IORef ~ Ref m) => Object IORef -> m (Maybe Environment)
objectToEnv = runMaybeT . flip properListOf \case
  Pair ref -> pure ref
  _ -> empty

objectToState
  :: (MonadMutableRef m, IORef ~ Ref m) => Object (Ref m) -> m (Either String EvalState)
objectToState = properList >=> \case
  Nothing -> pure $ Left $ "not a proper list"
  Just [g, v] -> bisequence (toKVPair g, toKVPair v) >>= \case
    (Just ("globe", g'), Just ("vmark", Pair v')) -> do
      base <- emptyState
      objectToEnv g' <&> \case
        Nothing -> Left $ "'globe' was not a valid environment"
        Just e -> Right $ base { _vmark = v', _globe = e }
    _ -> pure $ Left $ "state is missing one or both of 'globe' and 'vmark' fields"
  Just _ -> pure $ Left $ "wrong number of items in state"

stateToObject :: (MonadRef m, IORef ~ Ref m) => EvalState -> m (Object (Ref m))
stateToObject s = listToObject
  [ pure (Sym 'g' "lobe") >< envToObject (_globe s)
  , pure (Sym 'v' "mark") >< pure (Pair (_vmark s))
  ]

stringToState :: (MonadMutableRef m, Ref m ~ IORef) => String -> m (Either String EvalState)
stringToState s = parse "" s >>= \case
  Left err -> pure $ Left $ errorBundlePretty err
  Right obj -> objectToState obj

stateToString :: (MonadRef m, Ref m ~ IORef) => EvalState -> m String
stateToString = stateToObject >=> repr
