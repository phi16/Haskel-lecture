{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Stream as S
import Data.Function
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Stream

myRepeat :: a -> [a]
myRepeat x = fix $ \y -> x:y
fact :: Integer -> Integer
fact = fix $ \f x -> if x == 0
  then 1
  else x * f (x-1)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
through :: a -> a
through = loop swap

func :: Int -> Int
func = loop $ \(x,y) -> (y+1,x*2)
myRepeat2 :: a -> [a]
myRepeat2 = loop $ \(x,b) -> (b,x:b)
myRepeat3 :: a -> [a]
myRepeat3 = loop $ snd &&& uncurry (:)

fact2 :: Integer -> Integer
fact2 = loop $ \(x,f) -> (f x,) $ \y -> if y == 0
  then 1
  else y * f (y-1)
fact3 :: Integer -> Integer
fact3 = proc x -> do
  rec
    f <- returnA -< \y -> if y == 0
      then 1
      else y * f (y-1)
  returnA -< f x

sumStream :: StreamMap Int Int
sumStream = proc x -> do
  rec
    a <- returnA -< x + s
    s <- delay 0 -< a
  returnA -< a

fibStream :: StreamMap () Integer
fibStream = proc () -> do
  rec
    x <- delay 1 -< y
    y <- delay 1 -< x+y
  returnA -< x

sums :: S.Stream Int -> S.Stream Int
sums xs = runStream (arr snd >>> sumStream) ((),xs)

fibs :: S.Stream Integer
fibs = runStream (arr snd >>> fibStream) ((),S.repeat ())

sumNats :: [Int]
sumNats = S.toList $ sums $ S.fromList [1..]

fibsList :: [Integer]
fibsList = S.toList fibs