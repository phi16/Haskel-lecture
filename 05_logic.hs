import Prelude hiding (lookup)
import Data.List hiding (insert,lookup)
import Data.Map hiding (foldr,map,empty)
import qualified Data.Map as M (empty)
import Control.Applicative
import Control.Monad

class (Monad m, Alternative m) => Solver m where
  step :: m a -> m a

instance Solver [] where
  step x = x

newtype Leveled a = Leveled [[a]]
  deriving (Show)
unLeveled :: Leveled a -> [[a]]
unLeveled (Leveled x) = x
lzw :: (a -> a -> a) -> [a] -> [a] -> [a]
lzw e [] ys = ys
lzw e xs [] = xs
lzw e (x:xs) (y:ys) = x`e`y : lzw e xs ys
joins :: [[[[a]]]] -> [[a]]
joins = map concat . diag . map concatAll where
  concatAll = foldr (lzw (++)) []
  diag [] = []
  diag (x:xs) = lzw (++) (map return x) ([] : diag xs)
merge :: [[a]] -> [[a]] -> [[a]]
merge = lzw (++)
instance Functor Leveled where
  fmap f (Leveled xss) = Leveled $ map (map f) xss
instance Applicative Leveled where
  pure x = Leveled [[x]]
  Leveled f <*> Leveled x = Leveled $ joins $ map (map (\x' -> map (map (\f' -> f' x')) f)) x
instance Monad Leveled where
  return = pure
  Leveled x >>= f = Leveled $ joins $ map (map (unLeveled . f)) x
instance Alternative Leveled where
  empty = Leveled []
  Leveled x <|> Leveled y = Leveled $ merge x y
instance Solver Leveled where
  step (Leveled xs) = Leveled $ []:xs

select :: Solver m => [a] -> m a
select [] = empty
select (x:xs) = step $ return x <|> select xs

data Term = Int Int | Nil | Cons Term Term | Var Var
  deriving (Eq,Ord)
data Var = VarName String | VarGen Int
  deriving (Eq,Ord)

var :: String -> Term
var xs = Var $ VarName xs
list :: [Int] -> Term
list = foldr Cons Nil . map Int

newtype Subst = Subst (Map Var Term)

idSubst :: Subst
idSubst = Subst M.empty

extend :: (Var,Term) -> Subst -> Subst
extend (v,t) (Subst s) = Subst $ insert v t s

apply :: Subst -> Term -> Term
apply s i@(Int _) = i
apply s Nil = Nil
apply s (Cons a b) = Cons (apply s a) (apply s b)
apply (Subst s) (Var v) = case lookup v s of
  Nothing -> Var v
  Just e -> apply (Subst s) e

unify :: (Term,Term) -> Subst -> Maybe Subst
unify (x,y) s = case (apply s x,apply s y) of
  (Int x,Int y)
    | x == y -> Just s
  (Nil,Nil) -> Just s
  (Cons x xs,Cons y ys) -> do
    s' <- unify (x,y) s
    unify (xs,ys) s'
  (Var x,Var y)
    | x == y -> Just s
  (Var x,y)
    | not $ have x y s -> Just $ extend (x,y) s
  (x,Var y)
    | not $ have y x s -> Just $ extend (y,x) s
  (_,_) -> Nothing

have :: Var -> Term -> Subst -> Bool
have x t s = case apply s t of
  Var y -> x == y
  Cons y ys -> have x y s || have x ys s
  _ -> False

newtype Solution = Solution (Subst,Int)
type Predicate m = Solution -> m Solution

initial :: Solution
initial = Solution (idSubst,0)
run :: Solver m => Predicate m -> m Solution
run p = p initial

(===) :: Solver m => Term -> Term -> Predicate m
(x === y) (Solution (s,n)) = case unify (x,y) s of
  Just s' -> return $ Solution (s',n)
  Nothing -> empty

(&&&) :: Solver m => Predicate m -> Predicate m -> Predicate m
(x &&& y) s = do
  s' <- x s
  y s'

(|||) :: Solver m => Predicate m -> Predicate m -> Predicate m
(x ||| y) s = x s <|> y s

exists :: Solver m => (Term -> Predicate m) -> Predicate m
exists p (Solution (s,n)) = p (Var $ VarGen n) (Solution (s,n+1))

infixr 4 ===
infixr 3 &&&
infixr 2 |||

instance Show Var where
  show (VarName s) = s
  show (VarGen x) = "Gen" ++ show x
instance Show Term where
  show Nil = "[]"
  show (Cons x xs) = "[" ++ show x ++ unlist xs ++ "]" where
    unlist (Cons y ys) = "," ++ show y ++ unlist ys
    unlist Nil = ""
    unlist x = "|" ++ show x
  show (Int x) = show x
  show (Var v) = show v
instance Show Subst where
  show (Subst m) = "{" ++ intercalate ", " (sol m) ++ "}" where
    sol = foldrWithKey (\k a s -> conv k a ++ s) []
    conv (VarGen x) a = []
    conv (VarName x) a = return $ x ++ " = " ++ show (apply (Subst m) a)
instance Show Solution where
  show (Solution (a,_)) = show a

weight :: Solver m => Predicate m -> Predicate m
weight p s = step $ p s

append :: Solver m => Term -> Term -> Term -> Predicate m
append p q r = weight $ base ||| ind where
  base = p === Nil &&& q === r
  ind = exists $ \x -> exists $ \a -> exists $ \b -> p === Cons x a &&& r === Cons x b &&& append a q b 
