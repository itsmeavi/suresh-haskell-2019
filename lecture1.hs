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

xor :: Bool -> Bool -> Bool
xor b1 b2 = ((not b1) && b2) || (b1 && (not b2))

isOrdered :: Int -> Int -> Int -> Bool
isOrdered x y z = (x <= y) && (y <= z)