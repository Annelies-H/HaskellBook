module EitherMonad where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)  
  
instance Monoid a => Applicative (Sum a) where
  pure x = Second x
  Second f <*> Second x = Second (f x)
  First x <*> First y = First (mappend x y)
  First x <*> _ = First x
  _ <*> First x = First x
  
instance Monoid a => Monad (Sum a) where
  return = pure
  First x >>= _ = First x
  Second x >>= f = f x