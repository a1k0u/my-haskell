import           Data.List.Split
import           Data.Set                       ( fromList
                                                , toList
                                                )

type Symb = String
infixl 2 :@
infix 1 `alphaEq`
infix 1 `betaEq`

data Expr = Var Symb
          | Expr :@ Expr
          | Lam Symb Expr
          deriving (Eq)

uni :: [Symb] -> [Symb]
uni = toList . fromList

freeVars :: Expr -> [Symb]
freeVars (Var x            ) = [x]
freeVars (x   :@ y         ) = uni $ (freeVars x) ++ (freeVars y)
freeVars (Lam x  expression) = uni $ filter (\e -> x /= e) (freeVars expression)

setNewName :: Symb -> [Symb] -> [Symb] -> Symb
setNewName x n p | x `elem` n || x `elem` p = setNewName (x <> "'") n p
                 | otherwise                = x

subst :: Symb -> Expr -> Expr -> Expr
subst v n (Var x) | v == x    = n
                  | otherwise = Var x

subst v n (x :@ y) = subst v n x :@ subst v n y

subst v n (Lam x p) | v == x       = Lam x p
                    | x `elem` fvN = Lam x' (subst v n (subst x (Var x') p))
                    | otherwise    = Lam x (subst v n p)
 where
  x'         = setNewName x fvN fvP
  (fvN, fvP) = (freeVars n, freeVars p)

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x   ) (Var y   ) = x == y
alphaEq (x1 :@ x2) (y1 :@ y2) = alphaEq x1 y1 && alphaEq x2 y2
alphaEq (Lam x x1) (Lam y y1)
  | x == y             = alphaEq x1 y1
  | not $ y `elem` fvX = alphaEq (subst x (Var y) x1) y1
  | otherwise          = False
  where fvX = freeVars x1
alphaEq _ _ = False

reduceOnce :: Expr -> Maybe Expr
reduceOnce (Var _          ) = Nothing
reduceOnce ((Lam x xs) :@ y) = Just $ subst x y xs
reduceOnce (x          :@ y) = case x' of
  Nothing -> case y' of
    Nothing -> Nothing
    Just m  -> Just $ x :@ m
  Just m -> Just $ m :@ y
  where (x', y') = (reduceOnce x, reduceOnce y)
reduceOnce (Lam x xs) = case xs' of
  Nothing -> Nothing
  Just m  -> Just $ Lam x m
  where xs' = reduceOnce xs

nf :: Expr -> Expr
nf x = case x' of
  Nothing -> x
  Just m  -> nf m
  where x' = reduceOnce x

betaEq :: Expr -> Expr -> Bool
betaEq x y = nf x `alphaEq` nf y

buildLamSeq :: (String, Expr) -> (String, Expr)
buildLamSeq (str, expr) = case expr of
  Lam y ys -> buildLamSeq (str ++ y ++ " ", ys)
  _        -> (str, expr)

instance Show Expr where
  show (Var x) = x
  show (Var x :@ Var y) = x ++ " " ++ y
  show (Var x :@ m) = x ++ " " ++ "(" ++ show m ++ ")"

  show (m@(Lam _ _) :@ Var x) = "(" ++ show m ++ ")" ++ " " ++ x
  show (m@(Lam _ _) :@ n) = "(" ++ show m ++ ")" ++ " " ++ "(" ++ show n ++ ")"

  show (Lam x xs) = "\\" ++ lambdaVars ++ "-> " ++ show restExpression
    where (lambdaVars, restExpression) = buildLamSeq ("", Lam x xs)

  show (x :@ y) = show x ++ " " ++ show y

getContentInBrackets :: Integer -> [String] -> ([String], [String])
getContentInBrackets _ [] = ([], [])
getContentInBrackets weight (token : tokens)
  | token == ")" = if weight - 1 == 0
    then ([], tokens)
    else ([token] ++ tokenM', tokensM')
  | token == "(" = ([token] ++ tokenP', tokensP')
  | otherwise = ([token] ++ token', tokens')
 where
  (tokenM', tokensM') = getContentInBrackets (weight - 1) tokens
  (tokenP', tokensP') = getContentInBrackets (weight + 1) tokens
  (token' , tokens' ) = getContentInBrackets weight tokens

getElementsUntil :: [String] -> String -> ([String], [String])
getElementsUntil [] _ = ([], [])
getElementsUntil (token : tokens) e | token /= e = ([token] ++ token', tokens')
                                    | otherwise  = ([], tokens)
  where (token', tokens') = getElementsUntil tokens e

groupForApplication :: [String] -> [[String]]
groupForApplication [] = [[]]
groupForApplication (token : tokens)
  | token == "\\" = [["\\"] ++ lamAbstractor ++ ["->"] ++ lamBody]
  | token == "(" = if appTokens'' /= []
    then [appTokens'] ++ groupForApplication appTokens''
    else [appTokens']
  | otherwise = if tokens /= []
    then [[token]] ++ groupForApplication tokens
    else [[token]]
 where
  (appTokens'   , appTokens'') = getContentInBrackets 1 tokens
  (lamAbstractor, lamBody    ) = getElementsUntil tokens "->"

makeGroupApplication :: Expr -> [Expr] -> Expr
makeGroupApplication src []       = src
makeGroupApplication src (x : xs) = makeGroupApplication (src :@ x) xs

splitVar :: String -> [Char] -> (String, [Char])
splitVar acc [] = (acc, "")
splitVar acc (x : xs) | x `elem` "()\\" = (acc, x : xs)
                      | otherwise       = splitVar (acc ++ [x]) xs

splitElement :: [Char] -> [String]
splitElement []          = undefined
splitElement ("->"     ) = ["->"]
splitElement (')'  : x ) = if x /= [] then [")"] ++ splitElement x else [")"]
splitElement ('\\' : x ) = ["\\", x]
splitElement ('('  : x ) = if x /= [] then ["("] ++ splitElement x else ["("]
splitElement (x    : xs) = if rest /= ""
  then [var] ++ splitElement rest
  else [var]
  where (var, rest) = splitVar "" (x : xs)

splitLambdaExpression :: [Char] -> [String]
splitLambdaExpression xs = concat [ splitElement x | x <- splitOn " " xs ]

buildLambda :: [String] -> Expr
buildLambda [] = undefined
buildLambda (token : tokens)
  | token == "(" = if appTokens'' /= []
    then buildLambda appTokens' :@ buildLambda appTokens''
    else buildLambda appTokens'
  | token == "\\" = lambdaFunctions lamBodyExpression
  | otherwise = makeGroupApplication (Var token) applicationList
 where
  (appTokens'   , appTokens'') = getContentInBrackets 1 tokens

  (lamAbstractor, lamBody    ) = getElementsUntil tokens "->"
  lamBodyExpression            = buildLambda lamBody
  (x : xs)                     = [ Lam e | e <- lamAbstractor ]
  lambdaFunctions              = foldl (.) x xs

  applicationList              = if tokens == []
    then []
    else [ buildLambda e | e <- groupForApplication tokens ]

instance Read Expr where
  readsPrec _ xs = [(buildLambda $ splitLambdaExpression xs, "")]
