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

data Reg = Chr Bool Char
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
  elm = (Chr False <$> isChar isAlpha) <|> do
    char '('
    e <- reg
    char ')'
    return e

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

epsilon :: Reg -> Bool
epsilon (Chr b c) = False
epsilon (Seq l r) = epsilon l && epsilon r
epsilon (Alt l r) = epsilon l || epsilon r
epsilon (Rep p) = True
epsilon Eps = True
complete :: Reg -> Bool
complete (Chr b c) = b
complete (Seq l r) = complete l && epsilon r || complete r
complete (Alt l r) = complete l || complete r
complete (Rep p) = complete p
complete Eps = False
shift :: Bool -> Reg -> Char -> Reg
shift e (Chr b c) s = Chr (e && s == c) c
shift e (Seq l r) s = Seq (shift e l s) (shift (e && epsilon l || complete l) r s)
shift e (Alt l r) s = Alt (shift e l s) (shift e r s)
shift e (Rep p) s = Rep $ shift (e || complete p) p s
shift e Eps s = Eps

match :: Reg -> String -> Bool
match r [] = epsilon r
match r (c:cs) = complete $ foldl (shift False) ini cs where
  ini = shift True r c
  