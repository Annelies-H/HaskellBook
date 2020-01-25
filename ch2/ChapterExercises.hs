--
-- parenthesization

p1  = 2 + 2 * 3 - 1
p1' = 2 + (2 *3 ) - 1

p2   = (^) 10 $ 1 + 1
p2'  = (^) 10 2
p2'' = 10 ^ 2

p3  = 2 ^2 * 4 ^ 5 + 1
p3' = (2^2) * (4^5) + 1

--
-- equivalent expression

-- 1. equivalent, both evaluate to 2
-- 2. equivalent, 10 ^2 = 10*10 = 10 + 9*10
-- 3. not equivalent (-) 37 400 = 37 - 400 not 400 - 37
-- 4. equivalent
-- 5. not equivalent 2 * (5+18) = 2*23

--
-- more fun with functions


-- 1. '10 + waxOn' and '(+10) waxOn' add 10 to waxOn,
--    so 10 + waxOn = 10 + x * 5 = 10 + (y^2) * 5 = 10 + (z+8)^2  * 5
--                  = 10 + (7+8)^2 * 5 = 10 +  225 * 5 = 1135

-- 3. 'triple waxOn' multiplies waxOn by 3, because waxOn plays the role of x in the triple function

waxOn2 = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7
        
triple x = x * 3

waxOff x = triple x

-- 7. waxOff 10 = 10 * 3 = 30
-- 7. waxOff (-50) = (-50) * 3 = -150        