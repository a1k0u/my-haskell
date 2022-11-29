{-# LANGUAGE MultiParamTypeClasses #-}
import           Control.Applicative
import           Control.Monad
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
import Control.Monad (liftM, ap)
import           Control.Monad.Except
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Identity         ( Identity(..) )
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Char                      ( isNumber
                                                , isPunctuation
                                                , ord
                                                )
import           Data.Foldable

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (msum)
import Data.Char (isNumber, isPunctuation)

newtype StrRdrT m a = StrRdrT { runStrRdrT :: String -> m a }

-- instance Monad m => Monad (StrRdrT m) where
--   return :: a -> StrRdrT m a
--   return x = StrRdrT $ \_ -> return x

--   (>>=) :: StrRdrT m a -> (a -> StrRdrT m b) -> StrRdrT m b
--   (StrRdrT x) >>= k = StrRdrT $ \s -> do
--     y <- x s
--     let val = runStrRdrT $ k y
--     val s

-- instance MonadFail m => MonadFail (StrRdrT m)  where
--   fail :: String -> StrRdrT m a
--   fail s = StrRdrT $ \_ -> fail s

-- instance Monad m => Functor (StrRdrT m) where
--   fmap = liftM

-- instance Monad m => Applicative (StrRdrT m) where
--   pure  = return
--   (<*>) = ap

--------------------------------------
askStrRdr :: Monad m => StrRdrT m String
askStrRdr = StrRdrT $ \s -> pure s

asksStrRdr :: Monad m => (String -> a) -> StrRdrT m a
asksStrRdr f = StrRdrT $ \s -> pure $ f s

type StrRdr = StrRdrT Identity

runStrRdr :: StrRdr a -> String -> a
runStrRdr (StrRdrT f) env = runIdentity $ f env

--------------------------------------
-- instance MonadTrans StrRdrT where
--   lift v = StrRdrT $ \_ -> v

-- --------------------------------------
-- instance MonadState s m => MonadState s (StrRdrT m) where
--   get = lift get

--   put :: s -> StrRdrT m ()
--   put s = lift $ put s

--   state :: (s -> (a, s)) -> StrRdrT m a
--   state a = lift $ state a

-- --------------------------------------
-- class Monad m => MonadStrRdr m where
--   askSR :: m String
--   asksSR :: (String -> a) -> m a
--   strRdr :: (String -> a) -> m a

-- instance Monad m => MonadStrRdr (StrRdrT m) where
--   askSR :: StrRdrT m String
--   askSR = askStrRdr
--   asksSR :: (String -> a) -> StrRdrT m a
--   asksSR = asksStrRdr
--   strRdr :: (String -> a) -> StrRdrT m a
--   strRdr = asksStrRdr

-- instance MonadStrRdr m => MonadStrRdr (StateT s m) where
--   askSR :: StateT s m String
--   askSR = lift $ askSR
--   asksSR :: (String -> a) -> StateT s m a
--   asksSR f = lift $ asksSR f
--   strRdr :: (String -> a) -> StateT s m a
--   strRdr f = lift $ asksSR f

-- ------------------------------

-- data ListIndexError =
--   ErrTooLargeIndex Int
--   | ErrNegativeIndex
--   | OtherErr String
--   deriving (Eq, Show)

-- infixl 9 !!!
-- (!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
-- xs !!! n = getElement xs n n

-- getElement :: MonadError ListIndexError m => [a] -> Int -> Int -> m a
-- getElement [] n 0 = throwError $ ErrTooLargeIndex n
-- getElement [] n i | i < 0     = throwError ErrNegativeIndex
--                   | otherwise = throwError $ ErrTooLargeIndex n
-- getElement (x : xs) n 0 = pure x
-- getElement (x : xs) n i | i < 0     = throwError ErrNegativeIndex
--                         | otherwise = getElement xs n (i - 1)

-- ------------------------------

-- data Excep a =  Err String | Ok a
--   deriving (Eq, Show)

-- instance Functor Excep where
--   fmap :: (a -> b) -> Excep a -> Excep b
--   fmap _ (Err x) = Err x
--   fmap f (Ok  x) = Ok $ f x

-- instance Applicative Excep where
--   pure  = return
--   (<*>) = ap

-- instance Alternative Excep where
--   empty :: Excep a
--   empty = Err "Alternative.empty error."

--   (<|>) :: Excep a -> Excep a -> Excep a
--   (Ok  x) <|> _ = Ok x
--   (Err _) <|> y = y

-- instance Monad Excep where
--   return :: a -> Excep a
--   return = Ok

--   (>>=) :: Excep a -> (a -> Excep b) -> Excep b
--   (Err x) >>= _ = Err x
--   (Ok  x) >>= k = k x

-- instance MonadFail Excep where
--   fail :: String -> Excep a
--   fail _ = Err "Monad.fail error."

-- instance MonadPlus Excep where
--   mzero :: Excep a
--   mzero = Err "MonadPlus.fail error."

--   mplus :: Excep a -> Excep a -> Excep a
--   mplus = (<|>)

-- instance (MonadError String) Excep where
--     throwError = Err

--     (catchError) (Err x) k = k x
--     (catchError) m _ = m


-- (?/) :: (MonadError String m) 
--             => Double -> Double -> m Double
-- _ ?/ 0 = throwError "Division by 0."
-- x ?/ y = return $ x / y

-- example :: Double -> Double -> Excep String
-- example x y = action  `catchError` return where 
--   action = do 
--     q <- x ?/ y
--     guard (q >=0)
--     if q  > 100 then do 
--       100 <- return q
--       undefined
--     else 
--       return $ show q

------------------------------


data ParseError = ParseError
  { location :: Int
  , reason   :: String
  }
  deriving Show
type ParseMonad = Either ParseError

getHexInteger :: Char -> Integer
getHexInteger x = toInteger $ ord x - 48

getHexLetter :: Char -> Integer
getHexLetter x = toInteger $ ord x - 55

getHex :: String -> Integer -> Int -> ParseMonad Integer
getHex [] acc _ = Right $ acc
getHex (x : xs) acc pos
  | '0' <= x && x <= '9' = getHex xs (acc' + getHexInteger x) pos'
  | 'A' <= x && x <= 'F' = getHex xs (acc' + getHexLetter x) pos'
  | otherwise            = Left $ (ParseError pos' [x])
 where
  acc' = acc * 16
  pos' = pos + 1

parseHex :: String -> ParseMonad Integer
parseHex s = getHex s 0 0

printError :: ParseError -> ParseMonad String
printError err =
  return
    $  "At pos "
    ++ show (location err)
    ++ ": "
    ++ reason err
    ++ ": "
    ++ "invalid digit"

---------------------------------

-- askPassword :: PwdErrorMonad ()
-- askPassword = do
--   liftIO $ putStrLn "Enter your new password:"
--   value <- msum $ repeat getValidPassword
--   liftIO $ putStrLn "Storing in database..."

-- getValidPassword :: PwdErrorMonad String
-- getValidPassword = undefined

---------------------------------

-- data Logged a = Logged String a
--   deriving (Eq, Show)

-- instance Functor Logged where
--   fmap f (Logged x y) = Logged x (f y)

-- instance Applicative Logged where
--   pure  = return
--   (<*>) = ap

-- instance Monad Logged where
--   return = Logged mempty
--   (Logged s1 a1) >>= f = Logged (s2 ++ s1) a2 where Logged s2 a2 = f a1

-- -- write2log :: String -> Logged ()
-- -- write2log s = Logged s ()

-- newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

-- instance Monad m => Functor (LoggT m) where
--   fmap = liftM

-- instance Monad m => Applicative (LoggT m) where
--   pure = return
--   (<*>) = ap

-- instance Monad m => Monad (LoggT m) where
--   return x = LoggT $ return $ Logged mempty x
--   m >>= k = LoggT $ do
--     Logged x1 y1 <- runLoggT m
--     Logged x2 y2 <- runLoggT (k y1)
--     let z = Logged (x2 <> x1) y2
--     return $ z

-- instance MonadFail m => MonadFail (LoggT m) where
--   fail = LoggT . fail

-- write2log :: Monad m => String -> LoggT m ()
-- write2log s = LoggT (return (Logged s ()))

-- type Logg = LoggT Identity

-- runLogg :: Logg a -> Logged a
-- runLogg x = runIdentity (runLoggT x)

-- instance MonadTrans LoggT where
--   lift x = LoggT (fmap (Logged mempty) x)

-- instance MonadState s m => MonadState s (LoggT m) where
--     get   =  LoggT $ gets $ Logged mempty
--     put   = (LoggT . fmap (Logged mempty)) . put
--     state = (LoggT . fmap (Logged mempty)) . state


-- instance MonadReader r m => MonadReader r (LoggT m) where
--   ask        = lift $ ask
--   local      = mapLoggT . local
--   reader x   = lift $ reader x

-- mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
-- mapLoggT f (LoggT x) = LoggT $ f x

-- class Monad m => MonadLogg m where
--   w2log :: String -> m ()
--   logg :: Logged a -> m a

-- instance Monad m => MonadLogg (LoggT m) where
--   w2log   = write2log
--   logg  x = LoggT $ return x

-- instance MonadLogg m => MonadLogg (StateT s m) where
--   w2log s = lift $ w2log s
--   logg    = lift . logg

-- instance MonadLogg m => MonadLogg (ReaderT r m) where
--   w2log s = lift $ w2log s
--   logg    = lift . logg


-- Не снимайте комментарий - эти объявления даны в вызывающем коде

askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s

data PwdError = PwdError String

type PwdErrorMonad = ExceptT PwdError IO

askPassword :: PwdErrorMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."
  
getValidPassword :: PwdErrorMonad String
getValidPassword = undefined