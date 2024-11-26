import Data.Char

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
    | b1 == b2 = False                                              -- each return statement (after '=') guarded by a condition (after '|') 
    | b1 /= b2 = True            


greaterOrLessThan9 :: Int -> IO ()
greaterOrLessThan9 x
    | x >= 9 = putStrLn "greater than or equal"                     -- guards are not necessarily exhaustive (second guard falls under first)
    | x == 9 = putStrLn "equal"                                     -- Returns first statement that matches guard.
    | x < 9 = putStrLn "less than"                                  -- with argument 9, returns "greater than or equal"

--      Non-exhaustion can leave out condition in interpret time but create error in runtime
xorNonExhaustGuardRuntimeError :: Bool -> Bool -> Bool
xorNonExhaustGuardRuntimeError b1 b2                                -- guards may not exhaust all combos of input
    | b1 == True = not b2                                           -- will compile but show error at runtime
    | b2 == False = b1                                              -- this function does not have return for b1 = False, b2 = True


xorGuardOtherwise :: Bool -> Bool -> Bool
xorGuardOtherwise b1 b2                                             
    | b1 == True = not b2
    | otherwise = b2                                                -- use otherwise to catch all condition not caught by previous guards


-- Cases
xorCases :: Bool -> Bool -> Bool
xorCases b1 b2 = case b1 of                                         -- "case arg of.." is an expression returning boolean value
                    False -> b2                                     -- It is not a statement
                    True -> not b2

--      Nested Cases
xorNestedCases :: Bool -> Bool -> Bool
xorNestedCases b1 b2 = case b1 of
                        True -> case b2 of
                                    True -> False
                                    False -> True
                        
                        False -> case b2 of
                                    True -> True
                                    False -> False



-- Multiple Inputs
plusMultiInp :: Int -> Int -> Int
plusMultiInp n m = n + m

-- plusMultiInp n m => "plus" takes in "n", returns a function "plus n". 
--              Then "plus n" takes in "m", returns "n + m" 
-- Hence Int -> Int -> Int <==> Int -> (Int -> Int)
-- Similarly when calling func x1 x2 x2 <==> (((func x1) x2) x3)


---------------------------------------------------Lecture 4--------------------------------
-- Recursion
factorial :: Integer -> Integer                                     -- Int is bounded. Integer is arbitrary magnitude
factorial 0 = 1
factorial n
    | n < 0 = n * factorial (n+1)
    | n > 0 = n * factorial (n-1)


factInt :: Int -> Int                                               -- factInt 56 will overflow
factInt 0 = 1
factInt n 
    | n < 0 = n * factInt (n + 1)
    | n > 0 = n * factInt (n - 1)


hcf :: Integer -> Integer -> Integer
hcf b a 
    | b < 0 = -1
    | a < 0 = -1
    | b `mod` a == 0 = a                                            -- Binary func "mod a b" <==> a `mod` b as infix
    | otherwise = hcf a (b `mod` a)


-- Sectioning
-- (+) m n <==> m + n
-- (m+) n <==> function that adds m to n where m is first arg
-- (m/) n <==> function that divides m by n
-- (/m) n <==> function that divides arg by m


-- Local definitions
largestDiv :: Integer -> Integer                                    -- Given n, find the largest number less than n that divides n
largestDiv n = divSearch n (n-1)

divSearch :: Integer -> Integer -> Integer                          -- Given n, m; find largest number at most m that divides n
divSearch n m 
    | n`mod` m == 0 = m
    | otherwise = divSearch n (m-1)


--      divSearch ONLY required for largestDiv, hence can define locally
largestDiv2 :: Integer -> Integer
largestDiv2 n = divSearchLocal n (n-1)
            where
            divSearchLocal :: Integer -> Integer -> Integer
            divSearchLocal n m
                | n `mod` m == 0 = m
                | otherwise = divSearchLocal n (m-1)



--      local definitions can use defined items inside context
largestDiv3 :: Integer -> Integer
largestDiv3 n = divSearchLocal (n-1)
            where
            divSearchLocal :: Integer -> Integer
            divSearchLocal m
                | n `mod` m == 0 = m
                | otherwise = divSearchLocal $ m - 1                -- divSearchLocal $ m - 1 <==> divSearchLocal (m-1); $ used for ()




countDigits :: Integer -> Integer
countDigits n
    | abs n < 10 = 1
    | otherwise = countDigits (n `div` 10) + 1


power :: Integer -> Integer -> Integer
power m 0 = 1
power m n = m * power m (n-1)


posIntReverse :: Integer -> Integer
posIntReverse n
    | n < 0 = -1
    | n < 10 = n
    | otherwise = (n `mod` 10)*(10 `power` (countDigits n - 1)) + posIntReverse (n `div` 10)




posIntReverseAccu :: Integer -> Integer
posIntReverseAccu n = revInto 0 n

revInto :: Integer -> Integer -> Integer
revInto a n
    | n == 0 = a
    | otherwise = 
        let (q,r) = divMod n 10
        in
        revInto ((a*10) + r) q



-------------------------------- Lecture 6 ------------------------------------

-- Lists
--  type of list of type T is [T]
--  a:[b, c, d] = [a, b, c, d]
--  [a, b, c, d] = a: (b :(c: (d: [])))
--  head [a, b, c, d] = a
--  tail [a, b, c, d] = [b, c, d]

--  adds one to every member
addOne :: [Int] -> [Int]
addOne l    
    | null l = []
    | otherwise = (head l + 1) : addOne (tail l)


--  length of integer list
lenIntList :: [Int] -> Int
lenIntList l
    | null l = 0
    | otherwise = 1 + lenIntList (tail l)

--      uses pattern matching
lenIntListPattern :: [Int] -> Int
lenIntListPattern [] = 0
lenIntListPattern (x:xs) = 1 + lenIntListPattern (xs)


addAtEnd :: Int -> [Int] -> [Int]
addAtEnd x [] = x:[]
addAtEnd x l = (head l):(addAtEnd x (tail l))

addAtEndPattern :: Int -> [Int] -> [Int]
addAtEndPattern y [] = y:[]
addAtEndPattern y (x:xs) = x:(addAtEndPattern y (xs))

attach :: [Int] -> [Int] -> [Int]                                   -- Built-in function '++'
attach [] l2 = l2
attach (x:xs) l2 = x:(attach (xs) l2)


valueAtPos :: [Int] -> Int -> Int
--valueAtPos [] i = -1
valueAtPos (x:xs) 0 = x
valueAtPos (y:ys) i = valueAtPos (ys) (i-1)

-- Throwing error

valueAtPosWithError :: [Int] -> Int -> Int                          -- Built-in function "!!"
valueAtPosWithError l i 
    | null l = error "Empty List"
    | i < 0 = error "Negative Index"
    | i > ((lenIntList l) - 1) = error "Out of scope index"
    | otherwise = valueAtPos l i 


--      List notations
-- [m..n] = [m, m+1, m+2,..., n]
--      [m..n] = [] --------------> n < m
-- [a,a+d..n] = [a, a+d, a+2d, ..., ~n] --------> may be decreasing seq (d<0)
--      [100, 98.. 80] = [100,98,96,94,92,90,88,86,84,82,80]


--      Reverse list
revList :: [Int] -> [Int]                                           -- Built-in function "reverse"
revList [] = []
revList (x:xs) = (revList (xs))++[x]

-- Reverse using accumulator

revListAccum :: [Int] -> [Int]                                      -- this is how reverse works
revListAccum l = listRevAccum l []
            where
            listRevAccum :: [Int] -> [Int] -> [Int]
            listRevAccum [] l = l
            listRevAccum (x:xs) l = listRevAccum (xs) (x:l)




--      Some other built-in functions over lists
-- sum list = sum over all elements of the list
-- init list = list of all the elements of list except last
-- last list = last element
-- head list = first element
-- tail list = all last ements but first
-- take n l = returns first n elements of l; if n > max index of l, the whole l
-- drop n l = returns all but first n elemetns; if n > max index of l, empty []
-- take n l ++ drop n l = l



--------------------------------- Lecture 8 -------------------------------------------------------------------
mySplitAt :: Int -> [Int] -> ([Int],[Int])
mySplitAt 0 l = ([],l)
mySplitAt n (x:xs) = (x:f, s)
                where
                (f,s) = mySplitAt (n-1) (xs)


----------- Data Type Char----------------
fromCapsToSmall :: Char -> Char
fromCapsToSmall x 
    | ord x > ord 'Z' || ord x < ord 'A' = error "Not appropriate input"
    | otherwise = chr (ord x + 32)


--      Some built-in function
--  isSpace, isUpper, isLower, isDigit, isAlpha, isAlphaNum
--  toLower, toUpper
--  digitToInt, intToDigit


--      Strings
--  type String = [Char]

occurs :: String -> Char -> Bool                                    -- Built-in function elem t [t] is generalised for any lists
occurs [x] y = if x==y then True else False
occurs (x:xs) y = if x==y then True else occurs (xs) y 


capitalize :: String -> String
capitalize [x] = (toUpper x):[]
capitalize (a:as) = (toUpper a):(capitalize (as))


position :: Char -> String -> Maybe Integer
position _ "" = Nothing
position c (x:xs) 
    | c == x = Just 0
    | otherwise = case (position c (xs)) of
                    Nothing -> Nothing
                    Just i -> Just (i+1)


wordc :: String -> Integer                                          -- counts number of words in string
wordc s = counter 0 s + 1                                           -- wrong: counts consecutive spaces
        where                                                       -- wordc "ab   c" = 4
        counter :: Integer -> String -> Integer
        counter c "" = c
        counter c (x:xs)
            | isSpace x = counter (c+1) xs
            | otherwise = counter c xs



--          Corrected word count
wordc2 :: String -> Integer                                         
--wordc "" = 0
wordc2 s = counter 0 (' ':s) + 1                                    
        where
        counter :: Integer -> String -> Integer
        --counter c [x]
        counter c [x] = c
        counter c (x:y:ys)
            | isSpace y && isSpace x = counter c (y:ys)
            | isSpace y && not (isSpace x) = counter (c+1) (y:ys)
            | otherwise = counter c (y:ys)




-- Tuples
--      look up marks of a name
type MarksList = [(String, Integer)]
lookUpMarks :: String -> MarksList -> Maybe Integer
lookUpMarks _ [] = Nothing
lookUpMarks n ((name,marks):ml)
    | n==name = Just marks
    | otherwise = lookUpMarks n ml