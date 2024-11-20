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
