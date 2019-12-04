module Common where

import BasePrelude

bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

(<%>) :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
(<%>) = traverse
infixl 5 <%>
{-# INLINE (<%>) #-}

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip $ fmap . fmap
infixl 1 <&&>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixl 4 <$$>
{-# INLINE (<$$>) #-}

type String1 = NonEmpty Char
