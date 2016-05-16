{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Arrow
import Data.Char
import Data.List

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

data Reg = Chr Char
         | Seq Reg Reg
         | Alt Reg Reg
         | Rep Reg
         | Eps
  deriving (Show)

readReg :: String -> Maybe Reg
readReg str = parse reg str where
  reg :: Parser Reg
  reg = (eps >> return Eps) <|> do
    l <- trm
    v <- optional $ char '|' >> reg
    return $ case v of
      Nothing -> l
      Just r -> Alt l r
  trm :: Parser Reg
  trm = do
    l <- fct
    v <- optional trm
    return $ case v of
      Nothing -> l
      Just r -> Seq l r
  fct :: Parser Reg
  fct = do
    e <- elm
    v <- optional $ char '*'
    return $ case v of
      Nothing -> e
      Just _ -> Rep e
  elm :: Parser Reg
  elm = (Chr <$> isChar isAlpha) <|> do
    char '('
    e <- reg
    char ')'
    return e

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

match :: Reg -> String -> Bool
match (Chr c) s = [c] == s
match (Seq l r) s = any (\(x,y) -> match l x && match r y) $ parts s
match (Alt l r) s = match l s || match r s
match (Rep e) s = any (all $ match e) $ cuts s where
  cuts [] = [[]]
  cuts xs = tail (parts xs) >>= \(as,bs) -> (as:) <$> cuts bs
match Eps s = null s
