{-
    Double factorial.

    10!! = 2 * 4 * 6 * 8 * 10
    11!! = 11 * 9 * 7 * 5 * 3 * 1
-}

doubleFact :: Integer -> Integer
doubleFact n | n <= 2    = n
             | otherwise = n * doubleFact (n - 2)


{-
    Build a function that finds b_n for O(n).

    b_0​ = 1; b_1 ​= 2; b_2 ​= 3; b_{k+3}​ = b_{k+2}​ − 2b_{k + 1​} + 3b_{k}​.
-}

fstTrio :: (a, (a, a)) -> a
fstTrio = fst

sndTrio :: (a, (a, a)) -> a
sndTrio t = fst (snd t)

trdTrio :: (a, (a, a)) -> a
trdTrio t = snd (snd t)

moveTrio :: (a, (a, a)) -> a -> (a, (a, a))
moveTrio t = trio (sndTrio t) (trdTrio t)

trio :: a -> a -> a -> (a, (a, a))
trio x y z = (x, (y, z))

nextSeq :: Num a => (a, (a, a)) -> a
nextSeq t = trdTrio t - 2 * sndTrio t + 3 * fstTrio t

seqBHelper :: (Eq t, Num t, Num a) => (a, (a, a)) -> t -> a
seqBHelper trio n
  | n == 0    = trdTrio trio
  | otherwise = seqBHelper (moveTrio trio (nextSeq trio)) (n - 1)

seqB :: Integer -> Integer
seqB n | n == 0    = 1
       | n == 1    = 2
       | n == 2    = 3
       | otherwise = seqBHelper (trio 1 2 3) (n - 2)


{-
    Fibonacci in linear form.

    fibonacci (-99) -> 218922995834555169026
-}

accFib :: (Integer, Integer) -> Integer -> Integer
accFib accPair counter
  | counter == 0 = fst accPair
  | otherwise    = accFib (uncurry (+) accPair, fst accPair) (counter - 1)

fibonacci :: Integer -> Integer
fibonacci n | n < 0     = res * (-1) ^ mod (n + 1) 2
            | otherwise = res
  where res = accFib (0, 1) (goToPositive n)


{-
    Sum digits of number and count amount of digits.

    sum'n'count (-39) -> (12,2)
-}

goToPositive :: Integer -> Integer
goToPositive n | n < 0     = n * (-1)
               | otherwise = n

countDigits :: (Integer, Integer) -> Integer -> (Integer, Integer)
countDigits accPair n
  | n < 10    = (fst accPair + n, snd accPair + 1)
  | otherwise = countDigits (fst accPair + mod n 10, snd accPair + 1) (div n 10)

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = countDigits (0, 0) (goToPositive x)


{-
    Integration by trapezium method.

    integration sin pi 0 -> -2.0
-}

getStep :: Double -> Double -> Double -> Double
getStep l r p = (r - l) / p

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a == b    = 0
                  | a > b     = -integration f b a
                  | otherwise = (sumTrapeziums + t) * step
 where
  parts         = 1000
  sumTrapeziums = sum (map f [a + step, a + 2 * step .. b - step])
  t             = (f a + f b) / 2
  step          = getStep a b parts
