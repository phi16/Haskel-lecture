{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Arrow
import Control.Lens hiding (Empty)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State.Lazy hiding (State)
import Data.Char
import Data.Monoid
import System.Environment
import Text.Read hiding (lift,get)

newtype Cell = Cell Int
  deriving Eq

instance Num Cell where
  (Cell x) + (Cell y) = fromIntegral $ x+y
  (Cell x) * (Cell y) = fromIntegral $ x*y
  negate (Cell x) = fromIntegral $ -x
  abs (Cell x) = fromIntegral $ x
  signum (Cell x) = fromIntegral 1
  fromInteger x = Cell $ fromIntegral $ x`mod`256

instance Show Cell where
  showsPrec n (Cell x) = showsPrec n x

data Tape = T [Cell] Cell [Cell]

moveLeft :: Tape -> Tape
moveLeft (T l c r) = case l of
  [] -> T [] 0 (c:r)
  (p:ps) -> T ps p (c:r)

moveRight :: Tape -> Tape
moveRight (T l c r) = case r of
  [] -> T (c:l) 0 []
  (p:ps) -> T (c:l) p ps

increase :: Tape -> Tape
increase (T l c r) = T l (c+1) r

decrease :: Tape -> Tape
decrease (T l c r) = T l (c-1) r

readTape :: Tape -> Cell
readTape (T l c r) = c

writeTape :: Tape -> Cell -> Tape
writeTape (T l _ r) c = T l c r

data Code = Seq [Code] | Loop Code | Lef | Rht | Inc | Dec | In | Out
data ZipCode = Tape Code [Code] ZipCode [Code] | Circular ZipCode | Empty

next :: (Code, ZipCode) -> Maybe (Code, ZipCode)
next (Seq (x:xs),z) = Just (x,Tape (Seq (x:xs)) [] z xs)
next (Loop c,z) = Just (c,Circular z)
next (c,Tape v xs z []) = prev (v,z)
next (c,Tape v xs z (y:ys)) = Just (y,Tape v (c:xs) z ys)
next (c,Circular z) = Just (Loop c,z)
next (c,Empty) = Nothing

prev :: (Code, ZipCode) -> Maybe (Code, ZipCode)
prev (c,Circular z) = Just (Loop c,z)
prev (c,Tape v xs z []) = prev (v,z)
prev (c,Tape v xs z (y:ys)) = Just (y,Tape v (c:xs) z ys)
prev (c,Empty) = Nothing

instance Read Code where
  readsPrec n ys = let
      r ('<':xs) = [(Lef,xs)]
      r ('>':xs) = [(Rht,xs)]
      r ('+':xs) = [(Inc,xs)]
      r ('-':xs) = [(Dec,xs)]
      r (',':xs) = [(In,xs)]
      r ('.':xs) = [(Out,xs)]
      r ('[':xs) = do
        (v,vs) <- readsPrec n xs
        guard $ head vs == ']'
        return (Loop v,tail vs)
      r xs = []
      reading "" = [([],"")]
      reading (']':ys) = [([],']':ys)]
      reading ys = do
        (x,xs) <- r ys
        (vs,ts) <- reading xs
        return (x:vs,ts)
    in map (first Seq) $ reading ys

instance Show Code where
  showsPrec _ c = appEndo $ s c where
    s (Seq xs) = mconcat $ map s xs
    s (Loop x) = Endo ('[':) <> s x <> Endo (']':)
    s Lef = Endo ('<':)
    s Rht = Endo ('>':)
    s Inc = Endo ('+':)
    s Dec = Endo ('-':)
    s In  = Endo (',':)
    s Out = Endo ('.':)

type State = (Tape, Code, ZipCode)

makeTape :: Code -> State
makeTape c = (T [] 0 [],c,Empty)

type I a = StateT State IO a

input :: I Cell
input = lift $ fromIntegral . ord <$> getChar

output :: Cell -> I ()
output (Cell x) = lift $ putChar $ chr $ fromIntegral x

proc :: State -> I (Maybe State)
proc (t,c,z) = case c of
  (Seq _) -> return $ Just (t,c,z)
  (Loop _) -> do
    if readTape t /= 0
      then return $ Just (t,c,z)
      else case prev (c,z) of
        Just (c',z') -> proc (t,c',z')
        Nothing -> return Nothing
  _ -> Just <$> do
    t' <- case c of
      Lef -> return $ moveLeft t
      Rht -> return $ moveRight t
      Inc -> return $ increase t
      Dec -> return $ decrease t
      In  -> writeTape t <$> input
      Out -> output (readTape t) >> return t
    return (t',c,z)

main :: IO ()
main = do
  args <- getArgs
  src <- if length args == 0
    then getLine
    else fmap (head . lines) $ readFile $ head args
  let s = makeTape $ read src
  flip runStateT s $ fix $ \self -> do
    (t,c,z) <- get
    case next (c,z) of
      Just (c',z') -> do
        s' <- proc (t,c',z')
        case s' of
          Just s'' -> do
            put s''
            self
          Nothing -> return ()
      Nothing -> return ()
  return ()

