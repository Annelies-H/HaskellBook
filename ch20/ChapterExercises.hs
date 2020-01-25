module ChapterExercises where

import Data.Monoid

data Constant a b = Constant a deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant a) = z
  foldl f z (Constant a) = z
  foldMap f (Constant a) = mempty
  
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldl f z (Two a b) = f z b
  foldMap f (Two a b) = f b
  
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f z (Three a b c) = f c z
  foldl f z (Three a b c) = f z c
  foldMap f (Three a b c) = f c
  
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f z (Three' u v w) = f v (f w z)
  foldl f z (Three' u v w) = f (f z v) w
  foldMap f (Three' u v w) = f v <> f w
  
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f z (Four' a b c d) = f b $ f c $ f d z
  foldl f z (Four' a b c d) = f (f (f z d) c) b
  foldMap f (Four' a b c d) = f b <> f c <> f d
  
filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f xs = foldMap g xs
  where
    g x = if f x == True then (pure x) else mempty
