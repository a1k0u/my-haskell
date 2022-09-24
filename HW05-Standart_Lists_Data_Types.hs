-- Convert integer from negative to positive.
goToPositive :: Integer -> Integer
goToPositive n | n < 0     = n * (-1)
               | otherwise = n

-- Calculate amount of defite digit in integer array.
countDigits :: [Integer] -> Integer -> Integer
countDigits [] d = 0
countDigits (n : ns) d | n == d    = 1 + countDigits ns d
                       | otherwise = countDigits ns d

-- Convert integer into bool by some function.
convertIntBool :: Integer -> (Integer -> Bool) -> Bool
convertIntBool num func | func num  = True
                        | otherwise = False


{-
    Sums three array by each element.

    Here created all state for each array,
    it's like enumeration of all array conditions.

    sum3 [1,2,3] [4,5] [6] -> [11,7,3]
-}

sum3 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
sum3 []       []       []       = []

sum3 (x : xs) []       []       = (x + 0 + 0) : sum3 xs [] []
sum3 []       (y : ys) []       = (0 + y + 0) : sum3 [] ys []
sum3 []       []       (z : zs) = (0 + 0 + z) : sum3 [] [] zs

sum3 (x : xs) (y : ys) []       = (x + y + 0) : sum3 xs ys []
sum3 []       (y : ys) (z : zs) = (0 + y + z) : sum3 [] ys zs
sum3 (x : xs) []       (z : zs) = (x + 0 + z) : sum3 xs [] zs

sum3 (x : xs) (y : ys) (z : zs) = (x + y + z) : sum3 xs ys zs


{-
    Convert number into array of digits, similar to split number.

    Firstly convert number from negative to positive,
    then split it by recursion.

    digits 73643 -> [7, 3, 6, 4, 3]
-}

splitDigits :: Integer -> [Integer]
splitDigits n | n < 10    = [n]
              | otherwise = splitDigits (div n 10) ++ [mod n 10]

digits :: Integer -> [Integer]
digits n = splitDigits num where num = goToPositive n


{-
    containsAllDigits:
        Check out if all numbers from 1 to 9 contains in number.

    containsAllDigits (-123455556789) -> True

    containsAllDigitsOnes:
        The same as previous task, but every digit has to be only in number.

    containsAllDigitsOnes(-120003400567809) -> True

    Split number, start our counter with function to convert integer into bool.
-}

containsAllHelper :: [Integer] -> Integer -> (Integer -> Bool) -> Bool
containsAllHelper digits counter func
  | counter == 10
  = True
  | otherwise
  = convertIntBool (countDigits digits counter) func
    && containsAllHelper digits (counter + 1) func

containsAllDigits :: Integer -> Bool
containsAllDigits n = containsAllHelper d 1 (0 /=) where d = digits n

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n = containsAllHelper d 1 (1 ==) where d = digits n


{-
    Slicing list of [a, ...] from 'l' to 'r'.
    And returns this part.

    sublist 1 3 "abcdef"   -> "bc"
    sublist 0 10 [1, 2, 3] -> [1, 2, 3]
    sublist 3 10 [1..]     -> [4,5,6,7,8,9,10]
-}

sublistHelper :: Int -> Int -> [a] -> Int -> [a]
sublistHelper l r [] pos = []
sublistHelper l r (x : xs) pos
  | l <= pos && pos < r = x : sublistHelper l r xs (pos + 1)
  | pos < l             = sublistHelper l r xs (pos + 1)
  | otherwise           = []


sublist :: Int -> Int -> [a] -> [a]
sublist l r xs = sublistHelper l r xs 0


{-
    Repeat each element from list n-times,
    returns array of this repetition.

    repeatEveryElem 3 "abc"     -> "aaabbbccc"
    repeatEveryElem 2 [1, 2, 3] -> [1, 1, 2, 2, 3, 3]
-}

repeatElemHelper :: a -> Int -> Int -> [a]
repeatElemHelper el i cnt | i == cnt  = []
                          | otherwise = el : repeatElemHelper el (i + 1) cnt

repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem cnt = foldr (\x -> (++) (repeatElemHelper x 0 cnt)) []


{-
    The list is given (possibly infinite) and a positive integer.
    Create a of "sliding" sublists of length n that is,
    a list of lists of the following type:

    movingLists 2 [5..8] -> [[5,6],[6,7],[7,8]]
-}

createNList :: Int -> [a] -> [a]
createNList n [] = []
createNList n (x : xs) | n == 0    = []
                       | otherwise = x : createNList (n - 1) xs

movingLists :: Int -> [a] -> [[a]]
movingLists n [] = []
movingLists n xs | length list < n = []
                 | otherwise       = list : movingLists n (tail xs)
  where list = createNList n xs


{-
    Construct function cmp, comparing LogLevel elements
    with order: Error > Warning > Info.
-}

data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error   Error   = EQ
cmp Warning Warning = EQ
cmp Info    Info    = EQ

cmp Error   Warning = GT
cmp Error   Info    = GT
cmp Warning Error   = LT
cmp Info    Error   = LT

cmp Info    Warning = LT
cmp Warning Info    = GT


{-
    Implement the abbrFirstName function, which shortens the name
    to the first letter with a dot, that is, if the name was "John",
    then after applying this function, it will turn into "J.".
    However, if the name was shorter than two characters, then it does not change.

    ghci> let p = Person {firstName = "Adam", lastName = "Smith", age = 66}
    ghci> abbrFirstName p
    Person {firstName = "A.", lastName = "Smith", age = 66}
-}

data Person = Person
  { firstName :: String
  , lastName  :: String
  , age       :: Int
  }
  deriving Show

newName :: String -> String
newName name | length name <= 2 = name
             | otherwise        = head name : "."

abbrFirstName :: Person -> Person
abbrFirstName Person { firstName = f, lastName = l, age = a } =
  Person (newName f) l a


{-
    Tree       - binary tree.
    treeSum    - find sum of all elements (Nodes).
    treeHeight - find out tree height.

    GHCi> let tree = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
    GHCi> (treeSum tree, treeHeight tree)
    (10,3)
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf                = 0
treeSum (Node left x right) = treeSum left + x + treeSum right

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node left x right) =
  max (1 + treeHeight left) (1 + treeHeight right)
