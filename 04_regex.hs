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

data Reg s = Chr (Char -> s)
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
  elm = (Chr . (\c c' -> if c==c' then one else zero) <$> isChar isAlpha) <|> do
    char '('
    e <- reg
    char ')'
    return e

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

match :: Semiring r => Reg r -> String -> r
match (Chr c) s = case s of
  [x] -> c x
  _ -> zero
match (Seq l r) s = sum $ map (\(x,y) -> match l x `mul` match r y) $ parts s
match (Alt l r) s = match l s `add` match r s
match (Rep e) s = sum $ map (product . map (match e)) $ cuts s where
  cuts [] = [[]]
  cuts xs = tail (parts xs) >>= \(as,bs) -> (as:) <$> cuts bs
match Eps s = if null s then one else zero
