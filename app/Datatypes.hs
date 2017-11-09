import Data.Complex
import Data.Ratio
import Data.List
-- | We need to import "list" for intercalate, it will be needed later

-- | A regular mathematical integer (real whole number)
n0 :: Int
n0 = 5

-- | Decimal Number
n1 :: Double
n1 = 5

-- | Decimal complex number x + yi
n2 :: Complex Double
n2 = 2 :+ 3

-- | A simple ratio of two arbitrary-precision rational numbers
n3 :: Ratio Int
n3 = 2 % 3

-- | A standard char.. char uses single quotes
char0 :: Char
char0 = 'X'

-- | A decimal unicode char
char1 :: Char
char1 = '\0088'

-- | A hexidecimal unicode char
char2 :: Char
char2 = '\x0058'

-- | A octal unicode char
char3 :: Char
char3 = '\o0130'

-- | A standard string
string0 :: String
string0 = "This is a string"

-- | A combination of char representations forming a string.. strings require double quotes
string1 :: String
string1 = "\088\x0058\o00130"

-- | Lists are declared by placing [ ] around the type
intList :: [Int]
intList = [1,2,3,4,5]

stringList :: [String]
stringList = ["abc","r","c"] -- | This is not legal => ["abc", 'a', "c"] because the a is a Char

-- | A list of chars can just be a string
charList :: [Char]
charList = "abcd"
-- | It can also be a standard list of chars => charList = ['a', 'b', 'c', 'd']

-- | Another interesting list is an infinite list
infiniteList :: [Int]
infiniteList = [1..]

boundedList :: [Int]
boundedList = [1..10]

-- | Count by steps list .. provide the first two values, and then it will "step" the difference to the limit you set as the third
-- | The following will step by fours(5-1) till <=100.. so this will end at 97
steppedList :: [Int]
steppedList = [1,5..97]

-- | Tuples don't have to be the same types, they can combine any of the types to create a pair,triplet, or nested tuples in tuples
coolTuple :: (([Int], String), ([Float], [Char]))
coolTuple = (([1,2,3], "This is the tuple"), ([11.0,2.455], "Remember the charlist trick?")) -- | This might be the `trouble with tuples`

-- | Functions can be created with types as well
getStringLength :: String -> Int -- | Takes a string, gives an int
getStringLength = length

getStringAndLength :: String -> (String, Int) -- | Takes a string, gives the string and an int
getStringAndLength arg = (arg, length arg)

getStringsAndLengths :: [String] -> [(String, Int)] -- | Takes a string, gives the string and an int
getStringsAndLengths = map getStringAndLength

-- | You know what we need is more arrows.. lets get some
-- |listFormatter :: String -> String -> String -> [String] -> String
listFormatter :: String -> (String -> (String -> ([String] -> String))) -- | Can fully paranthsise and its still valid
listFormatter start end joiner stringList = start ++ (intercalate joiner (map show stringList)) ++ end -- | need to know more about "show"

-- | Polymorphism
mapping :: (a -> b) -> [a] -> [b]
mapping _ [] = []
mapping f (a:as) = f a : mapping f as -- | : constricting list
-- | The above function is polymorphic because it behaves thee same, no matter which types you provide
-- | I believe this is called parametric polymorphism.. alternatively there is "bounded" or "ad hoc" polymorphism in which you can overload the function for different argument types

-- | A little more about constraints
folder :: (a -> b -> b) -> b -> [a] -> b
folder _ b [] = b
folder f b (a:as) = folder f (f a b) as

-- | we can call folder likee this => print $ folder (+) 0 [1,2,3]
-- | but thanks to referential transparency, we can also create a "variable" for `folder (+) 0` and make it a little simpler
summer = folder (+) 0
callFolder = summer [1,2,3]
-- | Then we can just call `print $ callFolder`... ru-ru-ru-REFERENTIAL TRANSPARENCY!!!
-- | We didn't define a type for summer, since the use of the (+) constrains it, this is a little complicated and I don't fully understand it
-- | However a cool trick to figure out a suitable type signature would be to use the typed hole
-- | summer :: _
-- | On compile this will throw an error, but will also suggest a suitable type sig... Something like "Found type wildcard `_` standing for `[Integer] -> Integer`
-- | probably better not to be lazy though.. since this is at best a "guess"
-- | using this for summer will allow the program to compile correctly... TYPED HOLE!
-- | Further reading => https://wiki.haskell.org/Data_declaration_with_constraint



main :: IO ()
main = do
  print n0
  print n1
  print n2
  print n3
  print char0
  print char1
  print char2
  print char3
  print string0
  print string1
  print intList
  print stringList
  print charList
-- |  print infiniteList -- | probably shouldnt try to print this :p .. actually I did, nothing that a little CTRL+c won't fix
  print (take 20 infiniteList) -- | Cool trick to print only the first 20 of the infinite list
  print boundedList
  print steppedList
  print coolTuple
  -- | This syntax "applies" these functions to "arguments" and prints the results, since these functions don't have a "show"
  print $ getStringLength "How long is this thing anyway?"
  print $ getStringAndLength "How long is this thing anyway?"
  print $ getStringsAndLengths ["How", "long", "are", "these", "things", "anyway"]
  putStrLn $ listFormatter "<list>" "</list>" "|" ["First element", "Second element", "Third element"]
  print $ mapping show ["This is 1", "This is 2", "3"]
  -- | This will break by mixing datatypes => print $ mapping show ['1', "string"]  
  print $ callFolder
