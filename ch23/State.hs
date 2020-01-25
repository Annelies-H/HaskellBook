module State where

newtype Moi s a = 
  Moi { runMoi :: s -> (a , s) }
  
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)

foo :: Moi Int Bool
foo = (&&) <$> eerste <*> tweede
  where
    eerste = True <$ modify (+1)
    tweede = False <$ modify (+2)
  
instance Functor (Moi s) where
--fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ h.g
    where
      h (a, s) = (f a, s)
      
instance Applicative (Moi s) where
--pure :: a -> Moi s a
  pure  a = Moi (\s -> (a, s))
--(<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  Moi fab <*> Moi ga = Moi $ \s -> 
    let (f, newS) = fab s
        (a, newerS) = ga newS
    in (f a, newerS)
    
      


instance Monad (Moi s) where
  return = pure
--(>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  Moi sa >>= aSb = Moi $ \s -> 
    let (a, newS) = sa s
    in  runMoi (aSb a) newS

  

    
   