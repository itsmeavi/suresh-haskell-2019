succsr :: Int -> Int
succsr 0 = 1
succsr n = n+1

plusTwo n = succsr (succsr n)
plusThree n  = succsr (plusTwo n)

sqr :: Int -> Int
sqr x = x*x

plus :: Int -> Int -> Int
plus 1 m = succsr m
plus n m = succsr (plus (n-1) m)

mult :: Int -> Int -> Int
mult 0 m = 0 
mult n m = plus m (mult (n-1) m)

xor :: Bool -> Bool -> Bool                                         -- This line defines the type of function.
xor b1 b2 = ((not b1) && b2) || (b1 && (not b2))                    -- func :: T1 -> T2 -> T3 : func(T1, T2) = T3

isOrdered :: Int -> Int -> Int -> Bool
isOrdered x y z = (x <= y) && (y <= z)

-- lecture 2---

-- if-else
xorIfElse :: Bool -> Bool -> Bool                                   
xorIfElse b1 b2 = if b1==b2 then False else True                    -- After "then" and "else" defines return value

-- pattern matching
xorPattern1 :: Bool -> Bool -> Bool
xorPattern1 False b = b                                             -- Any call with
xorPattern1 b False = b                                             -- one False arg matches one of these lines
xorPattern1 b1 b2 = False                                           -- Other combos left (both True) matches this line

--      don't care
xorPattern2 :: Bool -> Bool -> Bool
xorPattern2 False b = b
xorPattern2 b False = b
xorPattern2 _ _     = False                                         -- Don't care argument (-).

returnEight :: Int -> Int
returnEight _ = 8                                                   -- Whatever you give, I don't care. I give you 8

-- Guarded functions
xorGuarded :: Bool -> Bool -> Bool
xorGuarded b1 b2
    | b1 == b2 = False
    | b1 /= b2 = True