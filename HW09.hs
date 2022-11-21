{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import           Control.Applicative
import           Data.Char

--

data Result a = Ok a | Error String
  deriving (Eq,Show)

instance Functor Result where
  fmap :: (a -> b) -> Result a -> Result b
  fmap _ (Error x) = Error x
  fmap f (Ok    a) = Ok (f a)

instance Foldable Result where
  foldMap :: Monoid m => (a -> m) -> Result a -> m
  foldMap _ (Error x) = mempty
  foldMap f (Ok    x) = f x

instance Traversable Result where
  traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
  traverse _ (Error x) = pure $ Error x
  traverse f (Ok    a) = Ok <$> f a

--

data NEList a = Single a | Cons a (NEList a)
  deriving (Eq,Show)

instance Functor NEList where
  fmap :: (a -> b) -> NEList a -> NEList b
  fmap f (Single a) = Single (f a)
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Foldable NEList where
  foldMap :: Monoid m => (a -> m) -> NEList a -> m
  foldMap f (Single x) = f x
  foldMap f (Cons x l) = f x <> foldMap f l

instance Traversable NEList where
  sequenceA :: Applicative f => NEList (f a) -> f (NEList a)
  sequenceA (Single x) = Single <$> x
  sequenceA (Cons x l) = Cons <$> x <*> sequenceA l


--

-- newtype Parser tok a =
--   Parser { runParser :: [tok] ->  Maybe ([tok],a) }

-- satisfy :: (tok -> Bool) -> Parser tok tok
-- satisfy pr = Parser f where
--   f (c:cs) | pr c  = Just (cs,c)
--   f _              = Nothing

-- lower :: Parser Char Char
-- lower = satisfy isLower

-- char :: Char -> Parser Char Char
-- char c = satisfy (== c)

-- digit :: Parser Char Int
-- digit = digitToInt <$> satisfy isDigit

-- instance Functor (Parser tok) where
--   fmap :: (a -> b) -> Parser tok a -> Parser tok b
--   fmap g (Parser p) = Parser $ (fmap . fmap . fmap) g p

-- instance Applicative (Parser tok) where
--   pure :: a -> Parser tok a
--   pure x = Parser $ \s -> Just (s, x)
--   (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
--   Parser u <*> Parser v = Parser f where
--     f xs = case u xs of
--       Nothing       -> Nothing
--       Just (xs', g) -> case v xs' of
--         Nothing        -> Nothing
--         Just (xs'', x) -> Just (xs'', g x)

-- multiplication :: Parser Char Int
-- multiplication = (*) <$> digit <* char '*' <*> digit


-- instance Alternative (Parser tok) where
--   empty :: Parser tok a
--   empty = Parser $ const Nothing
--   (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
--   Parser u <|> Parser v = Parser f where
--     f xs = case u xs of
--       Nothing -> v xs
--       z       -> z


-- nat = foldr (\r x -> r * 10 + x) 0 <$> some digit

--

data Triple a = Tr a a a
  deriving (Eq, Show)

instance Foldable Triple where
  foldl f ini (Tr x y z) = f (f (f ini x) y) z
  foldr f ini (Tr x y z) = f x (f y (f z ini))

instance Functor Triple  where
  fmap :: (a -> b) -> Triple a -> Triple b
  fmap f (Tr a1 a2 a3) = Tr (f a1) (f a2) (f a3)

instance Applicative Triple where
  pure :: a -> Triple a
  pure x = Tr x x x
  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  (Tr a1 a2 a3) <*> (Tr b1 b2 b3) = Tr (a1 b1) (a2 b2) (a3 b3)

instance Traversable Triple where
  traverse f (Tr x y z) = Tr <$> f x <*> f y <*> f z

--

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Nil            = Nil
  fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
  pure x = Branch (pure x) x (pure x)
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Branch l1 x1 r1) <*> (Branch l2 x2 r2) =
    Branch (l1 <*> l2) (x1 x2) (r1 <*> r2)

instance Foldable Tree where
  foldMap _ Nil            = mempty
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r


instance Traversable Tree where
  traverse _ Nil            = pure Nil
  traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

--

newtype Cmps f g x = Cmps { getCmps :: f (g x) }   deriving (Eq,Show)

instance (Foldable f, Foldable g) => Foldable (Cmps f g) where
  foldr f ini (Cmps x) = foldr (flip (foldr f)) ini x
  foldl f ini (Cmps x) = foldl (foldl f) ini x

--

instance (Functor f, Functor g) => Functor (Cmps f g) where
  fmap f (Cmps x) = Cmps $ fmap (fmap f) x

instance (Traversable f, Traversable g) => Traversable (Cmps f g) where
  traverse f (Cmps x) = Cmps <$> traverse (traverse f) x


--

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
  fmap = undefined

instance Applicative Parser where
  pure  = undefined
  (<*>) = undefined

instance Alternative Parser where
  empty = undefined
  (<|>) = undefined

--


class Functor f => Monoidal f where
  unit  :: f ()
  (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
  unit = Just ()
  (*&*) _        Nothing  = Nothing
  (*&*) Nothing  _        = Nothing
  (*&*) (Just x) (Just y) = Just (x, y)

instance Monoid s => Monoidal ((,) s) where
  unit = (mempty, ())
  (*&*) (x, y) (x', y') = (mappend x x', (y, y'))

instance Monoidal ((->) e) where
  unit = mempty
  (*&*) f g x = (f x, g x)

--

unit' :: Applicative f => f ()
unit' = pure ()

pair' :: Applicative f => f a -> f b -> f (a, b)
pair' a b = (,) <$> a <*> b

--

pure' :: Monoidal f => a -> f a
pure' a = fmap (const a) unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' a b = uncurry ($) <$> (*&*) a b
