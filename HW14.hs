{-# LANGUAGE InstanceSigs, StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

newtype Fix f = In (f (Fix f))

deriving instance Show (f (Fix f)) => Show (Fix f)
deriving instance Eq (f (Fix f)) => Eq (Fix f)

out :: Fix f -> f (Fix f)
out (In x) = x

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a 

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana psi x = In $ fmap (ana psi) (psi x)

hylo :: Functor f => Algebra f a -> Coalgebra f b -> (b -> a)
hylo phi psi = cata phi . ana psi

cata :: Functor f => Algebra f a -> Fix f -> a
cata phi (In x) = phi $ fmap (cata phi) x

--

data B x = Empty | One x | Zero x deriving (Eq,Show)

type Bin = Fix B

instance Functor B where
    fmap _ Empty = Empty
    fmap f (Zero x) = Zero $ f x
    fmap f (One x) = One $ f x

phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero x) = 2 * x
phiB (One x) = 2 * x + 1

bin2int :: Bin -> Int
bin2int = cata phiB

psiB :: Int -> B Int
psiB 0 = Empty
psiB x 
    | x `mod` 2 == 0 = Zero $ x `div` 2
    | otherwise = One $ x `div` 2

int2bin :: Int -> Bin
int2bin = ana psiB

--

-- data Expr = Num Int 
--      | Add Expr Expr 
--      | Mult Expr Expr

data E e = Num Int | Add e e | Mult e e

type Expr = Fix E

en = In . Num
e3     = en 3
ep35   = In (Add e3 (en 5)) 
emp357 = In (Mult ep35 (en 7))
em7p35 = In (Mult (en 7) ep35)

instance Functor E where
    fmap _ (Num x) = Num $ x
    fmap f (Add x y) = Add (f x) (f y)
    fmap f (Mult x y) = Mult (f x) (f y) 

phiE :: E Int -> Int
phiE (Num x) = x
phiE (Add x y) = x + y
phiE (Mult x y) = x * y

eval :: Expr -> Int
eval = cata phiE

phiEShow :: E String -> String
phiEShow (Num x) = show x
phiEShow (Add x y) = "(" ++ x ++ "+" ++ y ++ ")"
phiEShow (Mult x y) = "(" ++ x ++ "*" ++ y ++ ")"

--

space :: ShowS
space = showString " "

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num x) = shows x
phiEShowS (Add x y) = showString "+" . space . x . space . y
phiEShowS (Mult x y) = showString "*" . space . x . space . y

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add  (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num x) s = push x s
phiE' (Add x y) s = add $ x $ y s
phiE' (Mult x y) s = mult $ x $ y s

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'

--

iB l x r = In $ Branch l x r
iL = In Leaf

testTree = 
  iB
    (iB 
      (iB iL 
      2 
      iL) 
    3 
      (iB iL 
      4 
      iL)
    ) 
  5 
    (iB iL 
    6 
      (iB iL 
      7 
      iL)
    )


-- data Tree a = Leaf | Branch (Tree a) a (Tree a)

data T a x = Leaf | Branch x a x deriving(Show, Eq)

type Tree a = Fix (T a)

instance Functor (T a) where
    fmap _ Leaf = Leaf
    fmap f (Branch l x r) = Branch (f l) x (f r)

phiTSum :: Algebra (T Integer) Integer
phiTSum Leaf = 0
phiTSum (Branch l x r) = x + l + r

treeSum :: Tree Integer -> Integer
treeSum = cata phiTSum

--

phiTInorder :: Algebra (T a) [a] -- T a [a] -> [a]
phiTInorder Leaf = []
phiTInorder (Branch l x r) = l ++ [x] ++ r

tree2listInorder :: Tree a -> [a] 
tree2listInorder = cata phiTInorder

psiTBST :: Ord a => Coalgebra (T a) [a]    -- [a] -> T a [a] 
psiTBST [] = Leaf
psiTBST (x:xs) = Branch l x r
    where
        l = [v | v <- xs, v < x]
        r = [v | v <- xs, v >= x]

list2BST :: Ord a => [a] -> Tree a
list2BST = ana psiTBST

sort :: Ord a => [a] -> [a]
sort = hylo phiTInorder psiTBST