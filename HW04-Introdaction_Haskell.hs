-- Double factorial.

doubleFact :: Integer -> Integer
doubleFact n = if n <= 2 then n else n * doubleFact (n - 2) 

-- Sum digits of number and count amount of digits.

goToPositive :: Integer -> Integer
goToPositive n = if n < 0 then n * (-1) else n

countDigits :: (Integer, Integer) -> Integer -> (Integer, Integer)
countDigits accPair n = 
    if n < 10 
    then (
            (fst accPair) + n,
            (snd accPair) + 1
         ) 
    else 
        countDigits
            (
                (fst accPair) + (mod n 10),
                (snd accPair) + 1
            ) 
            (div n 10)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = countDigits (0, 0) (goToPositive x)

-- Fibonacci: exponensial and linear form.

fib' :: Integer -> Integer 
fib' n
   | n < 2 = n
   | otherwise = (fib' (n - 2)) +  (fib' (n - 1))


accFib :: (Integer, Integer) -> Integer -> Integer
accFib accPair counter  =
    if counter == 0
    then (fst accPair)
    else
        accFib
            ((fst accPair) + (snd accPair), fst accPair) (counter - 1)

fibonacci :: Integer -> Integer
fibonacci n = 
    let res = accFib (0, 1) (goToPositive n) 
    in 
        if n < 0 
        then res * (-1) ^ (mod (n + 1) 2) 
        else res


-- Integration by trapezium method.

getStep :: Double -> Double -> Double -> Double
getStep r l p = (r - l) / p 

sumTrapeziums f pos acc step iter iterStop = 
    if (iter == iterStop) 
    then acc
    else sumTrapeziums 
             f 
             (pos + step) 
             (acc + (f pos)) 
             step 
             (iter + 1) 
             iterStop

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let
        parts = 1000000  
        step = getStep a b parts
    in 
        (
            0.5 * (f a) +
            (sumTrapeziums f b 0 step 0 parts) +
            0.5 * (f b)
        ) * step

