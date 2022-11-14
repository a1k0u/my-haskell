{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}
import           Data.List
import           Control.Applicative (ZipList (ZipList), getZipList)

data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap _ (L x) = L x
  fmap f (R x) = R $ f x

instance Applicative (E l) where
  pure :: a -> E l a
  pure  = R
  (<*>) :: E l (a -> b) -> E l a -> E l b
  L x <*> _ = L x
  R f <*> x = f <$> x


newtype Cmps f g x = Cmps { getCmps :: f (g x) } deriving Show

ffmap h = getCmps . fmap h . Cmps

instance (Functor f, Functor g) => Functor (Cmps f g) where
    fmap :: (a -> b) -> Cmps f g a -> Cmps f g b
    fmap f (Cmps x) = Cmps $ fmap (fmap f) x



infixl 4 >$<
(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

infixl 4 >*<
(>*<) :: [a -> b] -> [a] -> [b]
x >*< y = getZipList $ ZipList x <*> ZipList y


data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple  where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Tr a1 a2 a3) = Tr (f a1) (f a2) (f a3)

instance Applicative Triple where
    pure :: a -> Triple a
    pure x = Tr x x x
    (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (Tr a1 a2 a3) <*> (Tr b1 b2 b3) = Tr (a1 b1) (a2 b2) (a3 b3)


data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f Nil            = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
    pure :: a -> Tree a
    pure x = Branch (pure x) x (pure x)
    (<*>) :: Tree (a -> b) -> Tree a -> Tree b
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Branch l1 x1 r1) <*> (Branch l2 x2 r2) = Branch (l1 <*> l2) (x1 x2) (r1 <*> r2)


instance (Applicative f, Applicative g) => Applicative (Cmps f g) where
    pure x = Cmps $ pure $ pure x
    Cmps f <*> Cmps g = Cmps $ (<*>) <$> f <*> g


divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' = foldr
      (\ x -> (<*>) ((/) <$> ("<-" ++ show x ++ "/", x))) ("1.0", 1)



newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
    fmap f (Arr2 g) = Arr2 (\e1 e2 -> f $ g e1 e2)

instance Functor (Arr3 e1 e2 e3) where
    fmap f (Arr3 g) = Arr3 (\e1 e2 e3 -> f $ g e1 e2 e3)

instance Applicative (Arr2 e1 e2) where
    pure x = Arr2 (\e1 e2 -> x)
    (Arr2 f) <*> (Arr2 g) = Arr2 (\e1 e2 -> f e1 e2 $ g e1 e2)

instance Applicative (Arr3 e1 e2 e3) where
    pure x = Arr3 (\e1 e2 e3 -> x)
    (Arr3 f) <*> (Arr3 g) = Arr3 (\e1 e2 e3 -> f e1 e2 e3 $ g e1 e2 e3)
