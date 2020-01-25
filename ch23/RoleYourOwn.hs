module RoleYourOwn where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)
  
intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic ex-tre-mely sparingly
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

-- this works but produces the same results everyt time
-- because its free of effects
-- though you can change the starting value to get new results    
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1,6)
  return (intToDie n, s)
 
example = evalState rollDie (mkStdGen 0)
  
rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

--repeat :: a -> [a]
--this doesnt work because the single die roll is repeated not the state action that produces a die
infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

--replicateM :: Monad m => Int -> m a -> m [a]
--this works because it repeats the action
nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
          | sum >= 20 = count
          | otherwise = 
            let (die, nextGen) = randomR (1,6) gen
            in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where go :: Int -> Int -> StdGen -> Int
        go sum count gen
           | sum >= n = count
           | otherwise =
             let (die, nextGen) = randomR (1,6) gen
             in go (sum + die) (count + 1) nextGen
             
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 (0, []) g
  where go :: Int -> (Int, [Int]) -> StdGen -> (Int, [Die])
        go sum (count , log) gen
           | sum >= n = (count, intToDie <$> reverse log)
           | otherwise =
             let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1, die:log) nextGen
             
example2 = rollsCountLogged 30 (mkStdGen 0)