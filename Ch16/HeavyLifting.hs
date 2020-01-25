module HeavyLifting where

a = fmap (+1) $ read "[1]" :: [Int]
 
b = (fmap . fmap) (++ "lol") (Just ["hi," , "hello"])

c = fmap (*2) (\x -> x - 2)
c' = (*2) . (\x -> x - 2)
 
d = ((return '1' ++) . show) . (\x -> [x, 1..3])
d' = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        iois :: IO String
        iois = fmap show ioi
        added :: IO String
        added = fmap ("123" ++) iois
        changed :: IO Integer
        changed = fmap read added
       -- changed = read ("123" ++) $ fmap show ioi
    in fmap (*3) changed
    
e' :: IO Integer
e' = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123"++) . show) ioi
     in fmap (*3) changed