import Data.Map
import System.IO

data Free f a = Free (f (Free f a)) | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free g) = Free $ fmap (fmap f) g
instance Functor f => Applicative (Free f) where
  pure a = Pure a
  Pure f <*> x = fmap f x
  Free g <*> x = Free $ fmap (<*>x) g
instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= g = g a
  Free f >>= g = Free $ fmap (>>=g) f

newtype Label = Label Int 
  deriving (Eq, Ord)
data Labeled a = Labeling (Label -> a) | Goto Label | Read (String -> a) | Print String a
instance Functor Labeled where
  fmap f (Labeling g) = Labeling (f.g)
  fmap f (Goto l) = Goto l
  fmap f (Read g) = Read (f.g)
  fmap f (Print xs c) = Print xs $ f c

type Goto = Free Labeled

evalGoto :: Goto a -> IO a
evalGoto a = exec 0 empty a where
  exec :: Int -> Map Label (Goto a) -> Goto a -> IO a
  exec l m (Pure x) = return x
  exec l m (Free (Labeling p)) = let
      cont = p $ Label l
      m' = insert (Label l) cont m
    in exec (l+1) m' cont  
  exec l m (Free (Goto c)) = exec l m $ m ! c
  exec l m (Free (Read f)) = getLine >>= exec l m . f
  exec l m (Free (Print s cont)) = putStrLn s >> hFlush stdout >> exec l m cont

label :: Goto Label
label = Free $ Labeling $ \l -> Pure l 
goto :: Label -> Goto ()
goto l = Free $ Goto l
input :: Goto String
input = Free $ Read $ \r -> Pure r
output :: String -> Goto ()
output xs = Free $ Print xs (Pure ())

main :: IO ()
main = evalGoto $ do
  f <- label
  output "First"
  s <- label
  output "Second"
  t <- label
  output "Third"
  u <- label
  output "Destination?"
  str <- input
  case str of
    "f" -> goto f
    "s" -> goto s
    "t" -> goto t
    "u" -> goto u
    _ -> output "Done!"

