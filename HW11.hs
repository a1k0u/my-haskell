import qualified Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.IORef
import           System.Random
--

data Logged a = Logged String a
  deriving (Eq, Show)

instance Functor Logged where
  fmap f (Logged x y) = Logged x (f y)

instance Applicative Logged where
  pure  = return
  (<*>) = ap

instance Monad Logged where
  return = Logged mempty
  (Logged s1 a1) >>= f = Logged (s2 ++ s1) a2 where Logged s2 a2 = f a1

write2log :: String -> Logged ()
write2log s = Logged s ()

--

updateLooger :: (Show a, Num a) => a -> (a, String) -> Writer String a
updateLooger e (v, s) = writer (e - v, ('(' : show e) ++ ('-' : s) ++ ")")

minusLoggedR' :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR' ini =
  foldr (\x rec_ -> updateLooger x (runWriter rec_)) (writer (ini, show ini))

foldMinusLoggedR :: (Show a, Num a) => a -> [a] -> (a, String)
foldMinusLoggedR ini (x : xs) =
  let (v, s) = foldMinusLoggedR ini xs
  in  (x - v, ('(' : show x) ++ ('-' : s) ++ ")")
foldMinusLoggedR ini [] = (ini, show ini)

minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR ini xs = writer (foldMinusLoggedR ini xs)

--

foldMinusLoggedL :: (Show a, Num a) => a -> String -> [a] -> (a, String)
foldMinusLoggedL ini s (x : xs) =
  foldMinusLoggedL (ini - x) (('(' : s) ++ ('-' : show x) ++ ")") xs
foldMinusLoggedL ini s [] = (ini, s)

minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL ini xs = writer (foldMinusLoggedL ini (show ini) xs)

--

fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0, 1)

fibStep :: State (Integer, Integer) ()
fibStep = do
  (previous, current) <- get
  put (current, (current + previous))



while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do
  i <- readIORef ref

  if (p i)
    then do
      action
      while ref p action
    else return ()


factorial :: Integer -> IO Integer
factorial n = do
  r <- newIORef 1
  i <- newIORef 1
  while
    i
    (<= n)
    (do
      ival <- readIORef i
      modifyIORef' r (* ival)
      modifyIORef' i (+ 1)
    )
  readIORef r

testIORef :: IO [Integer]
testIORef = do
  x    <- newIORef 1
  val1 <- readIORef x
  writeIORef x 41
  val2 <- readIORef x
  modifyIORef x succ
  val3 <- readIORef x
  return [val1, val2, val3]

--

{-
class Random a where
   randomR :: RandomGen g => (a, a) -> g -> (a, g)
   random  :: RandomGen g => g -> (a, g)

   randomRs :: RandomGen g => (a, a) -> g -> [a]
   randoms  :: RandomGen g => g -> [a]

   randomRIO :: (a,a) -> IO a
   randomIO  :: IO a
-}

fInt :: Int -> Double
fInt = fromIntegral

avgdev :: Int -> Int -> IO Double
avgdev k n =
  let
    n' = fInt n / 2
    k' = fInt k
  in
    (replicateM k $ replicateM n $ randomRIO (0, 1))
      >>= (\xs ->
            return $ (foldr (\x -> (+) $ abs $ fInt (sum x) - n') 0.0 xs) / k'
          )

--

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a
randomRState (a1, a2) = do
  g <- get
  let (a', g') = randomR (a1, a2) g
  put g'
  return a'

--

flipCoin :: Int -> Int -> [Int]
flipCoin n g = take n (randomRs (0, 1) (mkStdGen g))

countO :: [Int] -> Int
countO [] = 0
countO (x : xs) | x == 0    = 1 + countO xs
                | otherwise = countO xs

averageCount :: Int -> Int -> Int -> Double
averageCount _ _ 0 = 0
averageCount k n i =
  let n' = fromIntegral n / 2
      k' = fromIntegral k
  in  (abs $ n' - fromIntegral (countO $ flipCoin n i)) / k' + averageCount
        k
        n
        (i - 1)

rnd :: Double -> Double
rnd v = fromInteger (round (v * 1e6)) / 1e6

avgdev'' :: Int -> Int -> Double
avgdev'' k n = rnd $ averageCount k n k
