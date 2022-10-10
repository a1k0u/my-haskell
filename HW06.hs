{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import           Data.Complex
import           Data.List
import           Data.Maybe                     ( isNothing )
import           Distribution.Simple.Build      ( build )

newtype Matrix a = Matrix [[a]]

{-
    Make a type
        newtype Matrix a = Matrix [[a]]

    The rows of the matrix (internal lists) should be represented as lists;
    each subsequent internal list should start with a new line
    (use the character '\n' as a separator).
    An empty matrix should be output as EMPTY.

        GHCi> Matrix [[1,2,3],[4,5,6],[7,8,9]]
        [1,2,3]
        [4,5,6]
        [7,8,9]
        GHCi> Matrix []
        EMPTY
-}

instance Show a => Show (Matrix a) where
  show (Matrix x) | null x    = "EMPTY"
                  | otherwise = intercalate "\n" $ map show x

{-
    The Data.Complex module of the standard library
    implements the Complex a type of complex numbers.
    Make a type-wrapper

        newtype Complex = Cmplx (Complex Double) deriving Eq

    A representative of the Show type class must use the
    separators of the real and imaginary parts +i* and -i*,
    depending on the sign of the imaginary part:

        GHCi> Cmplx $ (-2.7) :+ 3.4
        -2.7+i*3.4
        GHCi> Cmplx $ (-2.7) :+ (-3.4)
        -2.7-i*3.4

-}
data Tree a = Leaf | Node (Tree a) a (Tree a)

elemTree :: Eq a => a -> Tree a -> Bool
elemTree x tree = check [tree]
 where
  check []          = False
  check (Leaf : xs) = check xs
  check ((Node l el r) : xs) | el == x   = True
                             | otherwise = check (xs ++ [l, r])

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  (/=) :: Tree a -> Tree a -> Bool
  tree1 == tree2 = go [tree1] [tree2]
   where
    go []          []          = True
    go (Leaf : xs) (Leaf : ys) = go xs ys
    go ((Node l1 x1 r1) : xs) ((Node l2 x2 r2) : ys)
      | x1 /= x2  = False
      | otherwise = go (xs ++ [l1, r1]) (ys ++ [l2, r2])

  tree1 /= tree2 = not (tree1 == tree2)


instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f t1 = buildTree f t1
   where
    buildTree f Leaf         = Leaf
    buildTree f (Node l x r) = Node (buildTree f l) (f x) (buildTree f r)


instance Show a => Show (Tree a) where
  show tree = buildTree tree
   where
    buildTree Leaf         = "{}"
    buildTree (Node l x r) = "<" ++ buildTree l ++ show x ++ buildTree r ++ ">"


instance Read a => Read (Tree a) where
  readsPrec _ ('{' : '}' : l) = [(Leaf, l)]
  readsPrec _ ('<' : rest1) =
    [ (Node l el r, s)
    | (l , rest2  ) <- reads rest1
    , (el, rest3  ) <- reads rest2
    , (r , '>' : s) <- reads rest3
    ]
  readsPrec _ _ = []

data List a = Nil | Cons a (List a)

instance Read a => Read (List a) where
  readsPrec _ input = myReadsList input


myReadsList :: (Read a) => ReadS (List a)
myReadsList ('|' : s) = [(Nil, s)]
myReadsList ('<' : s) =
  [ (Cons x l, u) | (x, t) <- reads s, (l, '>' : u) <- myReadsList t ]



-- myReadsTree :: (Read a) => ReadS (Tree a)
-- myReadsTree ('<':s) = [(Node l x r, u) |
--             (l, '>':p1) <- myReadsTree s,
--             (x, '<':p2) <- reads p1,
--             (r, '>':u) <- myReadsTree p2]


newtype Cmplx = Cmplx (Complex Double) deriving Eq

realPart' :: Cmplx -> Double
realPart' (Cmplx a) = realPart a

imagPart' :: Cmplx -> Double
imagPart' (Cmplx a) = imagPart a

instance Show Cmplx where
  show x = concat [show real, sign, show (abs imag)]
   where
    real = realPart' x
    imag = imagPart' x
    sign | imag < 0  = "-i*"
         | otherwise = "+i*"

instance Read Cmplx where
  readsPrec _ input =
    let (l, r)    = span (/= 'i') input
        imagValue = read (drop 2 r) :: Double
        signLeft | sign == '-' = -1
                 | otherwise   = 1
          where sign = head l

        real | signLeft == (-1) = drop 1 l
             | otherwise        = l

        (realValue, signRight)
          | last real == '-' = (read (takeWhile (/= '-') real) :: Double, -1)
          | otherwise        = (read (takeWhile (/= '+') real) :: Double, 1)
    in  [(Cmplx $ realValue * signLeft :+ imagValue * signRight, "")]


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = case drop n xs of
  [] -> rotateHelper (n `mod` length xs) xs
  _  -> rotateHelper n xs

rotateHelper 0 xs = xs
rotateHelper n xs | n > 0 = drop n xs ++ take n xs
                  | n < 0 = rotateHelper k xs
  where k = length xs - abs n


comb :: Int -> [a] -> [[a]]

comb 0 _        = [[]]
comb _ []       = []

comb 1 elements = [ [e1] | e1 <- elements ]
comb n elements =
  [ e1 : e2
  | (e1, i) <- zip elements [1 ..]
  , e2      <- comb (n - 1) (drop i elements)
  ]


-- instance (Read a) => Read (Tree a) where

--         readsPrec d r =  readParen (d > app_prec)
--                          (\r -> [(Leaf m,t) |
--                                  ("Leaf",s) <- lex r,
--                                  (m,t) <- readsPrec (app_prec+1) s]) r

--                       ++ readParen (d > up_prec)
--                          (\r -> [(u:^:v,w) |
--                                  (u,s) <- readsPrec (up_prec+1) r,
--                                  (":^:",t) <- lex s,
--                                  (v,w) <- readsPrec (up_prec+1) t]) r

--           where app_prec = 10
--                 up_prec = 5


class (Eq a, Enum a, Bounded a) => SafeEnum a where
    ssucc :: a -> a
    ssucc x
        | x == maxBound = minBound
        | otherwise = succ x

    spred :: a -> a
    spred x
        | x == minBound = maxBound
        | otherwise = pred x



-- revRange :: (Char,Char) -> String
-- revRange = unfoldr fun

-- fun (l, r) = if l <= r then Just (r, (l, pred r)) else Nothing

-- tails' :: [a] -> [[a]]
-- tails' = foldr fun ini
-- fun x y@(b:bx) = (x : b) : y
-- ini = [[]]

-- inits' :: [a] -> [[a]]
-- inits' = foldr fun' ini'
-- fun' x b = [] : map (x:) b
-- ini' = [[]]

-- take' :: Int -> [a] -> [a]
-- take' n xs = foldr step ini xs n
--  where
--   step :: a -> (Int -> [a]) -> Int -> [a]
--   step x g n | n <= 0    = g n
--              | otherwise = x : g (n - 1)
--   ini :: Int -> [a]
--   ini = const []

-- reverse' :: [a] -> [a]
-- reverse' = foldr fun' ini'
-- fun' x b = b ++ [x]
-- ini' = []

-- reverse'' :: [a] -> [a]
-- reverse'' = foldl fun'' ini''
-- fun'' x b = b : x
-- ini'' = []


-- (!!!) :: [a] -> Int -> Maybe a
-- xs !!! n = snd (foldr fun (length xs - n - 1, Nothing) xs)
-- fun x (i, el)
--   | i == 0 = (i - 1, Just x)
--   | isNothing el = (i - 1, Nothing)
--   | otherwise = (i - 1, el)

-- infixl 9 !!!


-- (!!!) :: (Foldable t1, Ord t2, Num t2) => t1 a -> t2 -> Maybe a
-- xs !!! n = foldr fun ini xs n
-- fun :: (Ord t, Num t) => a -> (t -> Maybe a) -> t -> Maybe a
-- fun x g n
--    | n < 0       = Nothing
--   | n == 0      = Just x
--   | otherwise   = g (n - 1)
-- ini = const Nothing


-- drop' :: Int -> [a] -> [a]
-- drop' n xs = foldr step ini xs n
-- step x g n
--   | n > 0 = g (n - 1)
--   | otherwise = x : g (n - 1)
-- ini = const []

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v
fun = undefined
ini = undefined


