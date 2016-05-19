{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (sum,product)
import Data.List hiding (sum,product)
import Data.Char
import Control.Applicative
import Control.Arrow

newtype Parser a = Parser (String -> [(a,String)])

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g
instance Applicative Parser where
  pure x = Parser $ \s -> [(x,s)]
  Parser f <*> Parser x = Parser $ \s -> do
    (f',s') <- f s
    (x',s'') <- x s'
    return (f' x',s'')
instance Monad Parser where
  return = pure
  Parser x >>= f = Parser $ \s -> do
    (x',s') <- x s
    let Parser f' = f x'
    f' s'
instance Alternative Parser where
  empty = Parser $ const []
  Parser x <|> Parser y = Parser $ \s -> x s ++ y s

isChar :: (Char -> Bool) -> Parser Char
isChar f = Parser $ \case
  [] -> []
  (x:xs)
    | f x -> [(x,xs)]
    | otherwise -> []

char :: Char -> Parser Char
char x = isChar (==x)

eps :: Parser ()
eps = Parser $ \xs -> [((),xs)]

parse :: Parser a -> String -> Maybe a
parse (Parser f) xs = safeHead $ filter (null.snd) $ f xs where
  safeHead [] = Nothing
  safeHead (x:xs) = Just $ fst x

class Semiring a where
  add :: a -> a -> a
  mul :: a -> a -> a
  zero :: a
  one :: a
  {- Laws
    <+> is commutative monoid with unit zero
    <*> is monoid with unit one
    a*(b+c) = a*b + a*c
    (a+b)*c = a*c + b*c
    zero is absorbing element of <*>
  -}
class Semiring a => Indexed c a where
  ix :: c -> Int -> a

sum :: Semiring a => [a] -> a
sum = foldr add zero
product :: Semiring a => [a] -> a
product = foldr mul one
fromBool :: Semiring a => Bool -> a
fromBool x = if x then one else zero

instance Semiring Bool where
  add = (||)
  mul = (&&)
  zero = False
  one = True
instance Indexed c Bool where
  ix _ _ = True

instance Semiring Int where
  add = (+)
  mul = (*)
  zero = 0
  one = 1
instance Indexed c Int where
  ix _ _ = 1

data Range = Fail | Empty | Range (Int,Int)
  deriving (Show)
instance Semiring Range where
  add Fail x = x
  add x Fail = x
  add Empty x = x
  add x Empty = x
  add (Range (i,j)) (Range (k,l))
    | i < k || i == k && j >= l = Range (i,j)
    | otherwise = Range (k,l)
  mul Fail x = Fail
  mul x Fail = Fail
  mul Empty x = x
  mul x Empty = x
  mul (Range (i,j)) (Range (k,l)) = Range (i,l)
  zero = Fail
  one = Empty
instance Indexed c Range where
  ix _ x = Range (x,x)

newtype Matches c = Matches (Int,[[c]])
  deriving (Show)
instance Ord c => Semiring (Matches c) where
  add (Matches (_,[])) y = y
  add x (Matches (_,[])) = x
  add (Matches (a,x)) (Matches (b,y))
    | a < b = Matches (b,y)
    | a > b = Matches (a,x)
    | otherwise = Matches (a,nub $ x++y)
  mul (Matches (a,x)) (Matches (b,y)) = Matches (a+b, liftA2 (++) x y)
  zero = Matches (undefined,[])
  one = Matches (0,[[]])
instance Ord c => Indexed c (Matches c) where
  ix c _ = Matches (1,[[c]])

data Reg c s = Chr (c -> s)
           | Seq (RegC c s) (RegC c s)
           | Alt (RegC c s) (RegC c s)
           | Rep (RegC c s)
           | Eps
data RegC c s = RegC {
  regC :: Reg c s,
  isEps :: s,
  isComp :: s
}

readReg :: Semiring r => String -> Maybe (RegC Char r)
readReg str = parse reg str where
  reg = (eps >> return epsC) <|> do
    l <- trm
    v <- optional $ char '|' >> reg
    return $ case v of
      Nothing -> l
      Just r -> altC l r
  trm = do
    l <- fct
    v <- optional trm
    return $ case v of
      Nothing -> l
      Just r -> seqC l r
  fct = do
    e <- elm
    v <- optional $ char '*'
    return $ case v of
      Nothing -> e
      Just _ -> repC e
  elm = chi <|> any <|> do
    char '('
    e <- reg
    char ')'
    return e
  chi = chrC . (\c c' -> fromBool $ c==c') <$> isChar isAlpha
  any = chrC . (\c -> const one) <$> char '.'

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

chrC :: Semiring r => (c -> r) -> RegC c r
chrC c = RegC (Chr c) zero zero
seqC :: Semiring r => RegC c r -> RegC c r -> RegC c r
seqC l r = RegC (Seq l r) (isEps l `mul` isEps r) ((isComp l `mul` isEps r) `add` isComp r)
altC :: Semiring r => RegC c r -> RegC c r -> RegC c r
altC l r = RegC (Alt l r) (isEps l `add` isEps r) (isComp l `add` isComp r)
repC :: Semiring r => RegC c r -> RegC c r
repC p = RegC (Rep p) one (isComp p)
epsC :: Semiring r => RegC c r
epsC = RegC Eps one zero

shift :: Semiring r => r -> Reg c r -> c -> RegC c r
shift e (Chr c) s = (chrC c) { isComp = e `mul` c s }
shift e (Seq l r) s = seqC (shiftR e l s) (shiftR ((e `mul` isEps l) `add` isComp l) r s)
shift e (Alt l r) s = altC (shiftR e l s) (shiftR e r s)
shift e (Rep p) s = repC $ shiftR (e `add` isComp p) p s
shift e Eps s = epsC
shiftR :: Semiring r => r -> RegC c r -> c -> RegC c r
shiftR e r s = shift e (regC r) s

match :: Semiring r => RegC c r -> [c] -> r
match r [] = isEps r
match r (c:cs) = isComp $ foldl (shiftR zero) ini cs where
  ini = shiftR one r c

indexing :: Indexed c r => RegC c r -> RegC (c,Int) r
indexing x = x { regC = ixing $ regC x } where
  ixing :: Indexed c r => Reg c r -> Reg (c,Int) r
  ixing (Chr c) = Chr $ \(s,i) -> c s `mul` ix s i
  ixing (Seq l r) = Seq (indexing l) (indexing r)
  ixing (Alt l r) = Alt (indexing l) (indexing r)
  ixing (Rep p) = Rep (indexing p)
  ixing Eps = Eps

submatch :: Indexed c r => RegC c r -> [c] -> r
submatch r s = match r' $ zip s [0..] where
  r' = anyR`seqC`indexing r`seqC`anyR
  anyR = repC $ chrC $ const one

test x y = fmap ((\t -> match t y)) $ readReg x
test' x y = fmap ((\t -> submatch t y)) $ readReg x