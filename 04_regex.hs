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

instance Semiring Int where
  add = (+)
  mul = (*)
  zero = 0
  one = 1

data Reg s = Chr s (Char -> s)
           | Seq (Reg s) (Reg s)
           | Alt (Reg s) (Reg s)
           | Rep (Reg s)
           | Eps

readReg :: Semiring r => String -> Maybe (Reg r)
readReg str = parse reg str where
  reg = (eps >> return Eps) <|> do
    l <- trm
    v <- optional $ char '|' >> reg
    return $ case v of
      Nothing -> l
      Just r -> Alt l r
  trm = do
    l <- fct
    v <- optional trm
    return $ case v of
      Nothing -> l
      Just r -> Seq l r
  fct = do
    e <- elm
    v <- optional $ char '*'
    return $ case v of
      Nothing -> e
      Just _ -> Rep e
  elm = (Chr zero . (\c c' -> fromBool $ c==c') <$> isChar isAlpha) <|> do
    char '('
    e <- reg
    char ')'
    return e

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

epsilon :: Semiring r => Reg r -> r
epsilon (Chr b c) = zero
epsilon (Seq l r) = epsilon l `mul` epsilon r
epsilon (Alt l r) = epsilon l `add` epsilon r
epsilon (Rep p) = one
epsilon Eps = one
complete :: Semiring r => Reg r -> r
complete (Chr b c) = b
complete (Seq l r) = (complete l `mul` epsilon r) `add` complete r
complete (Alt l r) = complete l `add` complete r
complete (Rep p) = complete p
complete Eps = zero
shift :: Semiring r => r -> Reg r -> Char -> Reg r
shift e (Chr b c) s = Chr (e `mul` c s) c
shift e (Seq l r) s = Seq (shift e l s) (shift ((e `mul` epsilon l) `add` complete l) r s)
shift e (Alt l r) s = Alt (shift e l s) (shift e r s)
shift e (Rep p) s = Rep $ shift (e `add` complete p) p s
shift e Eps s = Eps

match :: Semiring r => Reg r -> String -> r
match r [] = epsilon r
match r (c:cs) = complete $ foldl (shift zero) ini cs where
  ini = shift one r c
