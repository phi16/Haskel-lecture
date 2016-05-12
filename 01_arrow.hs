{-# LANGUAGE Arrows #-}

import qualified Data.Stream as S
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer.Stream

-- fix / loop

sumStream :: StreamMap Int Int
sumStream = proc x -> do
  rec
    a <- returnA -< x + s
    s <- delay 0 -< a
  returnA -< a

fibStream :: StreamMap () Int
fibStream = proc () -> do
  rec
    x <- delay 1 -< y
    y <- delay 1 -< x+y
  returnA -< x

sums :: S.Stream Int -> S.Stream Int
sums xs = runStream (arr snd >>> sumStream) ((),xs)

fibs :: S.Stream Int
fibs = runStream (arr snd >>> fibStream) ((),S.repeat ())

