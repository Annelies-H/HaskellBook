module ChapterExercises where

import State

get :: Moi s s
get = Moi (\x -> (x, x))

put :: s -> Moi s ()
put s = Moi $ \x -> ((), s)

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ runMoi (Moi sa) s
    
eval :: Moi s a -> s -> a
eval (Moi sa) = fst . runMoi (Moi sa) 

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)