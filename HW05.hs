{-
    Sums three array by each element.

    Create all state for each array,
    it's like enumeration of all conditions.
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

goToPositive :: Integer -> Integer
goToPositive n | n < 0     = n * (-1)
               | otherwise = n

splitDigits :: Integer -> [Integer]
splitDigits n | n < 10    = [n]
              | otherwise = splitDigits (div n 10) ++ [mod n 10]

digits :: Integer -> [Integer]
digits n = splitDigits num where num = goToPositive n

-- Containg

{-

-}


countDigits :: [Integer] -> Integer -> Integer
countDigits [] d = 0
countDigits (n : ns) d
    | n == d    = 1 + countDigits ns d
    | otherwise = countDigits ns d

convertIntBool :: Integer -> (Integer -> Bool) -> Bool
convertIntBool num func
    | func num  = True
    | otherwise = False

containsAllHelper :: [Integer] -> Integer -> (Integer -> Bool) -> Bool
containsAllHelper digits counter func
    | counter == 10 = True
    | otherwise     =
        convertIntBool (countDigits digits counter) func &&
            containsAllHelper digits (counter + 1) func

containsAllDigits :: Integer -> Bool
containsAllDigits n = containsAllHelper d 1 (0 /=) where d = digits n

-- Contains all digits ones

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes n = containsAllHelper d 1 (1 ==) where d = digits n
