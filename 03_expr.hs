-- Differentiate

{-# LANGUAGE LambdaCase #-}

import Data.List
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

char :: Char -> Parser Char
char c = Parser $ \case
  [] -> []
  (x:xs)
    | x == c -> [(c,xs)]
    | otherwise -> []

anyChar :: Parser Char
anyChar = Parser $ \case
  [] -> []
  (x:xs) -> [(x,xs)]

string :: String -> Parser String
string s = Parser $ \xs -> if s`isPrefixOf`xs
  then [(s,drop (length s) xs)]
  else []

digit :: Parser Int
digit = Parser $ \case
  [] -> []
  (x:xs)
    | isDigit x -> [(digitToInt x,xs)]
    | otherwise -> []

uint :: Parser Integer
uint = fmap (toInt.reverse) $ some digit where
  toInt :: [Int] -> Integer
  toInt [] = 0
  toInt (x:xs) = toInt xs * 10 + fromIntegral x

integer :: Parser Integer
integer = do
  c <- optional $ char '-'
  i <- uint
  case c of
    Nothing -> return i
    Just _ -> return $ -i

decimal :: Parser Double
decimal = do
  i <- fromIntegral <$> integer
  c <- optional $ char '.' >> many digit
  case c of
    Nothing -> return i
    Just vs -> return $ i + signum i * w where
      w = sum $ zipWith f vs [1..]
      f x y = fromIntegral x * 0.1 ^ y

runParser :: Parser a -> String -> [(a,String)]
runParser (Parser f) xs = f xs

parse :: Parser a -> String -> Maybe a
parse p xs = safeHead $ filter (null.snd) $ runParser p xs where
  safeHead [] = Nothing
  safeHead (x:xs) = Just $ fst x

eval :: String -> Maybe Double
eval = parse expr . filter (/=' ') where
  expr :: Parser Double
  expr = term <|> do
    l <- term
    c <- char '+' <|> char '-'
    r <- term
    return $ if c == '+' then l + r else l - r
  term :: Parser Double
  term = fact <|> do
    l <- fact
    c <- char '*' <|> char '/'
    r <- fact
    return $ if c == '*' then l * r else l / r
  fact :: Parser Double
  fact = decimal <|> do
    char '('
    e <- expr
    char ')'
    return e

data Expr = Num Double
          | X
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Fun Func Expr
  deriving Show

data Func = Neg | Abs | Sgn
          | Exp | Log | Sqr
          | Sin | Cos | Tan
  deriving Show

readExpr :: String -> Maybe Expr
readExpr = parse expr . filter (/=' ') where
  expr :: Parser Expr
  expr = term <|> do
    l <- term
    c <- char '+' <|> char '-'
    r <- expr
    return $ if c == '+' then Add l r else Sub l r
  term :: Parser Expr
  term = fact <|> do
    l <- fact
    c <- char '*' <|> char '/'
    r <- term
    return $ if c == '*' then Mul l r else Div l r
  fact :: Parser Expr
  fact = do
    l <- negs
    c <- optional $ char '^' >> fact
    return $ case c of
      Nothing -> l
      Just r -> Pow l r
  negs :: Parser Expr
  negs = do
    s <- optional $ char '-'
    l <- lits
    return $ case s of
      Nothing -> l
      Just _ -> Fun Neg l
  lits :: Parser Expr
  lits = fmap Num decimal <|> (char 'x' >> return X) <|> do
    f <- optional $ foldr1 (<|>) [
      string "abs" >> return Abs,
      string "sgn" >> return Sgn,
      string "exp" >> return Exp,
      string "log" >> return Log,
      string "sqrt" >> return Sqr,
      string "sin" >> return Sin,
      string "cos" >> return Cos,
      string "tan" >> return Tan]
    char '('
    e <- expr
    char ')'
    return $ case f of
      Nothing -> e
      Just u -> Fun u e

apply :: Expr -> Double -> Double