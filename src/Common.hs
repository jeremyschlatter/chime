module Common (module Common, module BasePrelude) where

import BasePrelude hiding (map)

bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:

(<%>) :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
(<%>) = traverse
infixl 5 <%>

(<&&>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<&&>) = flip $ map . map
infixl 1 <&&>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = map . map
infixl 4 <$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = map . map . map
infixl 4 <$$$>

type String1 = NonEmpty Char

unfinished :: a
unfinished = undefined
{-# WARNING unfinished "This code is unfinished" #-}

interpreterBug :: String -> a
interpreterBug = error . ("interpreter bug: " <>)
