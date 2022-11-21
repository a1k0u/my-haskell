import           Control.Monad

iffy :: Applicative f => f Bool -> f a -> f a -> f a
iffy fCond fThen fElse =
  (\b t e -> if b then t else e) <$> fCond <*> fThen <*> fElse

miffy :: Monad m => m Bool -> m a -> m a -> m a
miffy mCond mThen mElse = mCond >>= (\b -> if b then mThen else mElse)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = do
  x    <- xs
  True <- return (f x)
  return x

replicate' :: Int -> a -> [a]
replicate' i xs = do
  iter <- [0 .. i]
  return xs


isTiny x = x < 127 && x > -128

infixl 3 ?+?

(?+?) :: Int -> Int -> Maybe Int
x ?+? y | isTiny sum = pure sum
        | otherwise  = fail "The sum is out of bounds"
  where sum = x + y


myfmap :: Monad m => (a -> b) -> m a -> m b
myfmap f ma = ma >>= \x -> return $ f x

myfmap' :: Monad m => (a -> b) -> m a -> m b
myfmap' f ma = do
  a <- ma
  return $ f a

myapp :: Monad m => m (a -> b) -> m a -> m b
myapp mf ma = mf >>= (\f -> ma >>= (\a -> return $ f a))

myapp' :: Monad m => m (a -> b) -> m a -> m b
myapp' mf ma = do
  f <- mf
  a <- ma
  return $ f a

--

-- doNTurns :: Int -> Board -> [Board]
-- doNTurns 0 ini = [ini]
-- doNTurns n ini = do
--                     bdN <- next ini
--                     doNTurns (n - 1) bdN

--
surround' :: a -> a -> [a] -> [a]
surround' x y zs = concat $ [ ([x] ++ [z] ++ [y]) | z <- zs ]

surround :: a -> a -> [a] -> [a]
surround x y zs = concat $ do
  z <- zs
  return ([x] ++ [z] ++ [y])

--

lookups :: (Eq k) => k -> [(k, v)] -> [v]
lookups x ys = do
  (k, v) <- ys
  True   <- return $ x == k
  return $ v

--

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do
  d    <- [1 .. (isqrt n)]
  True <- return $ mod n d == 0
  return $ (,) d (div n d)

-- 

absDiff :: Num a => [a] -> [a]
absDiff []           = []
absDiff [x, y]       = [abs $ y - x]
absDiff (x : y : xs) = (abs $ y - x) : absDiff (y : xs)
absDiff _            = []


--

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x0 y0 z0) o1            o2 = Bi x0 y0 (concat3OC z0 o1 o2)
concat3OC (Un x0      ) (Un x1      ) o2 = Bi x0 x1 o2
concat3OC (Un x0      ) (Bi x1 y1 z1) o2 = Bi x0 x1 (concat3OC (Un y1) z1 o2)

--

concatOC :: OddC (OddC a) -> OddC a
concatOC (Bi x y z) = concat3OC x y $ concatOC z
concatOC (Un x    ) = x

--

instance Functor OddC where
  fmap f (Un x    ) = Un (f x)
  fmap f (Bi x y z) = Bi (f x) (f y) (fmap f z)


instance Applicative OddC where
  pure  = Un
  (<*>) = ap

instance Monad OddC where
  (Un x    ) >>= k = k x
  (Bi x y z) >>= k = concat3OC (k x) (k y) (z >>= k)
  return = pure

tst1 = Bi 10 20 (Un 30)
tst2 = Bi 1 2 (Bi 3 4 (Un 5))

