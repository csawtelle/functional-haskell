module Main where

import Lib

-- | Double colon "hastype"
-- | IO is a type class for input/output
-- | () unit, called unit, tuple with 0 elements
-- | Side effect is that the characters are written to the terminal


main :: IO ()


-- | Call the function you want from this line ex. main = helloWorld
-- | main helloWorld

-- | Adds 10 to each member of the list 1,2,3 results in 11,12,13
addTenToList = print $ map (+10) [1,2,3]

-- | Adds each element of the list to a sum, results in 6
foldList = print $ foldr (+) 0 [1,2,3]

-- | Filters the list 1,2,3,4,5,6 for elements >3, results in 4,5,6
filterList = print $ filter (>3) [1,2,3,4,5,6]

-- | Adds 10 to an argument X , results in x+10
addTen x = x + 10

-- | Multiply X by X, squaring it, results in x^2
squareX x = x * x

-- | Joins squareX and addTen to perform both on the list, results [121,144,169] 
addAndSquare = print $ map (squareX . addTen) [1,2,3]

-- | A function that checks if x is 5 and prints whether it is or isn't
isFive = 
  let x = 4
  in print $ myIf (x == 5) "is 5" "is not five"
myIf True thenFunc elseFunc = thenFunc
myIf False thenFunc elseFunc = elseFunc

-- | Classic hello world
helloWorld = putStrLn "Hello World"


-- | The difference between putStrLn and print.. putStrLn will maintain linebreaks
-- | print will print raw text such as newline characters \n inline
regularPrint = do
  content <- readFile "numbers.txt"
  print content

-- |
-- | We will try some much more complicated things, processing some numbers
-- |
-- | Convert a string to an Int
readInts :: String -> [Int]
readInts s = let ws = words s in map read ws

-- | Sort from minimum to maximum
minMax :: Ord a => [a] -> Maybe (a,a)
-- | if list has head or tail
minMax (h : t) = Just $ foldr
  (\x (min,max) -> (if x < min then x else min, if x > max then x else max))
  (h,h) 
  t
-- | if list is empty ( _ is like "else" in this pattern matching scheme )
minMax _ = Nothing

processNumbers = do
  -- | Read in the file to "content"
  content <- readFile "numbers.txt"
  -- | convert the strings to ints
  -- | Implied scope in do block
  let values = readInts content
      count = length values
      total = sum values
      -- | Must be converted from integral
      mean = fromIntegral total / fromIntegral count
      range = minMax values
  print count
  print total
  print mean
  print range

-- | Show the power of chaining functions with the dot operator
doubleIt x = x*2
chain = addTen . doubleIt
-- | funcChaining = print $ map chain [1,2,3,4,5]

-- | We can do the same thing with an anonymous function, without naming it "chain"
funcChaining = print $ map (addTen . doubleIt) [1,2,3,4,5]

-- | Using a lambda
-- | all three are equivalent
-- | \x y -> x + y
-- | \x -> \y -> x + y
-- | \x -> (\y -> x + y)
parenWords = print $ parenthesizeWords "we love haskell"

-- | parenthesizeWords s = unwords $ map parenthesizeWord (words s)
       -- | different s in child scope
-- |   where parenthesizeWord s  = "(" ++ s ++ ")"

-- | It can also be rewritten using a lambda as below
parenthesizeWords s = unwords $ map (\s -> "(" ++ s ++ ")") (words s)


main = parenWords







