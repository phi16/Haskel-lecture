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

data Reg s = Chr (Char -> s)
           | Seq (RegC s) (RegC s)
           | Alt (RegC s) (RegC s)
           | Rep (RegC s)
           | Eps
data RegC s = RegC {
  regC :: Reg s,
  isEps :: s,
  isComp :: s
}

readReg :: Semiring r => String -> Maybe (RegC r)
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
  elm = (chrC . (\c c' -> fromBool $ c==c') <$> isChar isAlpha) <|> do
    char '('
    e <- reg
    char ')'
    return e

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

chrC :: Semiring r => (Char -> r) -> RegC r
chrC c = RegC (Chr c) zero zero
seqC :: Semiring r => RegC r -> RegC r -> RegC r
seqC l r = RegC (Seq l r) (isEps l `mul` isEps r) ((isComp l `mul` isEps r) `add` isComp r)
altC :: Semiring r => RegC r -> RegC r -> RegC r
altC l r = RegC (Alt l r) (isEps l `add` isEps r) (isComp l `add` isComp r)
repC :: Semiring r => RegC r -> RegC r
repC p = RegC (Rep p) one (isComp p)
epsC :: Semiring r => RegC r
epsC = RegC Eps one zero

shift :: Semiring r => r -> Reg r -> Char -> RegC r
shift e (Chr c) s = (chrC c) { isComp = e `mul` c s }
shift e (Seq l r) s = seqC (shiftR e l s) (shiftR ((e `mul` isEps l) `add` isComp l) r s)
shift e (Alt l r) s = altC (shiftR e l s) (shiftR e r s)
shift e (Rep p) s = repC $ shiftR (e `add` isComp p) p s
shift e Eps s = epsC
shiftR :: Semiring r => r -> RegC r -> Char -> RegC r
shiftR e r s = shift e (regC r) s

match :: Semiring r => RegC r -> String -> r
match r [] = isEps r
match r (c:cs) = isComp $ foldl (shiftR zero) ini cs where
  ini = shiftR one r c
  