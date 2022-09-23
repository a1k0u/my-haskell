-- Practice 4
evenCount [] = 0
evenCount (x : xs) | even x    = 1 + evenCount xs
                   | otherwise = evenCount xs

onlyOdds [] = []
onlyOdds (x : xs) | odd x     = x : onlyOdds xs
                  | otherwise = onlyOdds xs
permuteOddsAndEvens [x] = [x]
permuteOddsAndEvens []  = []
permuteOddsAndEvens (x : y : xs)
    | odd x && even y = y : x : permuteOddsAndEvens xs
    | even x && odd y = y : x : permuteOddsAndEvens xs
    | otherwise       = x : y : permuteOddsAndEvens xs


-- Sums arrays by element

sum3 []       []       []       = []

sum3 (x : xs) []       []       = (x + 0 + 0) : sum3 xs [] []
sum3 []       (y : ys) []       = (0 + y + 0) : sum3 [] ys []
sum3 []       []       (z : zs) = (0 + 0 + z) : sum3 [] [] zs

sum3 (x : xs) (y : ys) []       = (x + y + 0) : sum3 xs ys []
sum3 []       (y : ys) (z : zs) = (0 + y + z) : sum3 [] ys zs
sum3 (x : xs) []       (z : zs) = (x + 0 + z) : sum3 xs [] zs

sum3 (x : xs) (y : ys) (z : zs) = (x + y + z) : sum3 xs ys zs

-- Split digits

goToPositive :: Integer -> Integer
goToPositive n | n < 0     = n * (-1)
               | otherwise = n

splitDigits :: Integer -> [Integer]
splitDigits n | n < 10    = [n]
              | otherwise = splitDigits (div n 10) ++ [mod n 10]

digits :: Integer -> [Integer]
digits n = splitDigits num where num = goToPositive n

-- Containg

containDigit [] d = False
containDigit (n : ns) d | n == d    = True
                        | otherwise = containDigit ns d


containsAllDigits :: Integer -> Bool
containsAllDigits n
    | containDigit d 1
        && containDigit d 2
        && containDigit d 3
        && containDigit d 4
        && containDigit d 5
        && containDigit d 6
        && containDigit d 7
        && containDigit d 8
        && containDigit d 9
    = True
    | otherwise
    = False
    where d = digits n




-- Contains all digits ones

