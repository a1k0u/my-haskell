{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Except
import           Data.List                      ( group
                                                , union
                                                )
import           Data.Set                       ( fromList
                                                , toList
                                                )

infixl 4 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)

-- Lam "x" (Var "x" :@ Var "x")

uni :: [Symb] -> [Symb]
uni = toList . fromList

freeVars :: Expr -> [Symb]
freeVars (Var x            ) = [x]
freeVars (x   :@ y         ) = uni $ (freeVars x) ++ (freeVars y)
freeVars (Lam x  expression) = uni $ filter (\e -> x /= e) (freeVars expression)

freeTVars :: Type -> [Symb]
freeTVars (TVar x ) = [x]
freeTVars (x :-> y) = uni xs where xs = freeTVars x ++ freeTVars y

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env e) x t = Env $ (x, t) : e

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env []           ) = []
freeTVarsEnv (Env ((_, t) : xs)) = uni ys
  where ys = (freeTVars t) ++ freeTVarsEnv (Env xs)

appEnv :: MonadError String m => Env -> Symb -> m Type
appEnv (Env []) v =
  throwError
    $  "There is no variable "
    ++ "\""
    ++ v
    ++ "\""
    ++ " in the environment."
appEnv (Env ((x, t) : xs)) v | v == x    = pure t
                             | otherwise = appEnv (Env xs) v

appSubsTy :: SubsTy -> Type -> Type
appSubsTy xs          (a :-> b) = (appSubsTy xs a) :-> (appSubsTy xs b)
appSubsTy (SubsTy xs) (TVar a ) = case v of
  Nothing -> TVar a
  Just b  -> b
  where v = lookup a xs

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv _ (Env []) = Env []
appSubsEnv subsTy (Env env) = Env [ (s, appSubsTy subsTy t) | (s, t) <- env ]

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy (SubsTy t) (SubsTy s) = SubsTy (ys ++ xs)
 where
  xs     = [ (v', (appSubsTy (SubsTy t) t')) | (v', t') <- s ]
  xsVars = [ v' | (v', _) <- xs ]
  ys     = [ (v', t') | (v', t') <- t, not (elem v' xsVars) ]

instance Semigroup SubsTy where
  (<>) = composeSubsTy

instance Monoid SubsTy where
  mempty = SubsTy []

unify :: MonadError String m => Type -> Type -> m SubsTy
unify (TVar a) (TVar b) | a == b    = return $ SubsTy []
                        | otherwise = return $ SubsTy [(a, TVar b)]
unify a'@(TVar a) t
  | a `elem` fv
  = throwError $ "Can't unify (" ++ show a' ++ ") with (" ++ show t ++ ")!"
  | otherwise
  = return $ SubsTy [(a, t)]
  where fv = freeTVars t
unify t''@(_  :-> _ ) t'@(TVar _   ) = unify t' t''
unify (    o1 :-> o2) (   t1 :-> t2) = do
  let u2 = unify o2 t2
  u2' <- u2
  let u1 = unify (appSubsTy u2' o1) (appSubsTy u2' t1)
  u1' <- u1
  return $ composeSubsTy u1' u2'

setNewName :: Symb -> Int -> Symb
setNewName x k = x ++ show k

equations :: MonadError String m => Env -> Expr -> Type -> m [(Type, Type)]
equations c m o = do
     (xs, _) <- equations' c m o 1
     return $ xs

equations' :: MonadError String m => Env -> Expr -> Type -> Int -> m ([(Type, Type)], Int)
equations' c (Var x) o k = do
  typeVar <- appEnv c x
  return $ ([(o, typeVar)], k)

equations' c (m :@ n) o k = do
  (xs, k') <- (equations' c m (TVar a :-> o) (k + 1))
  (ys, k'') <- (equations' c n (TVar a) (k'))
  return $ (xs `union` ys, k'')
  where a = setNewName "a" k

equations' c (Lam x m) o k = do
  (xs, k') <- equations' (extendEnv c x a) m (b) (k + 2)
  return $ (xs `union` [(a :-> b, o)], k')
 where
  a = TVar $ setNewName "a" k
  b = TVar $ setNewName "a" (k + 1)

concatTypes :: [(Type, Type)] -> (Type, Type)
concatTypes [] = undefined
concatTypes [(t1, t2)] = (t1, t2)
concatTypes ((t1, t2):xs) = (t1 :-> t1', t2 :-> t2')
  where
    (t1', t2') = concatTypes xs

principlePair :: MonadError String m => Expr -> m (Env, Type)
principlePair m = do
    let xs = equations env m a0
    xs' <- xs
    let (t1, t2) = concatTypes xs'
    let u = unify t1 t2
    u' <- u
    return $ (appSubsEnv u' env, appSubsTy u' a0)
  where
    fv = freeVars m
    types = [TVar ('b' : show n) | n <- [1..length fv]]
    env = Env (zip fv types)
    a0 = TVar "a0"
