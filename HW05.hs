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
-}

splitDigits :: Integer -> [Integer]
splitDigits n | n < 10    = [n]
              | otherwise = splitDigits (div n 10) ++ [mod n 10]

digits :: Integer -> [Integer]
digits n = splitDigits num where num = goToPositive n


{-
    containsAllDigits:
        Check out if all numbers from 1 to 9 contains in number.

    containsAllDigitsOnes:
        The same as previous task, but every digit has to be only in number.

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

-}

sliceList :: Int -> Int -> [a] -> Int -> [a]
sliceList l r [] pos = []
sliceList l r (x : xs) pos
  | l <= pos && pos < r = x : sliceList l r xs (pos + 1)
  | pos < l             = sliceList l r xs (pos + 1)
  | otherwise           = []


sublist :: Int -> Int -> [a] -> [a]
sublist l r xs | l >= r    = []
               | otherwise = sliceList l r xs 0
