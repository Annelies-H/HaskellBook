module Bind where

import Control.Monad (join)


andOne x = [x, 1] 
listy = [4,5,6]

justX x = Just x
justy = Just 23

-- bindOne :: Monad m => (a -> m b) -> m a -> m b
bindOne = join $ fmap andOne listy

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x