import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array
import Control.Monad
import Control.Lens hiding (rewrite)
import System.Random

w :: Int
w = 400

main :: IO ()
main = play (InWindow "po" (w,w) (w,w)) (greyN 0.5) 60 iniWorld render handle step

type State = Array (Int,Int) Bool

n :: Int
n = 15

sRange :: ((Int,Int),(Int,Int))
sRange = ((0,0),(n-1,n-1))

iniWorld :: State
iniWorld = listArray sRange $ cycle [False,True,True,True]

render :: State -> Picture
render a = pictures $ assocs a <&> \((x',y'),b) -> let
    x = fromIntegral x'
    y = fromIntegral y'
    s = fromIntegral w / fromIntegral n
    w' = fromIntegral w
    t = translate (x*s-w'/2+s/2) (y*s-w'/2+s/2)
    c = color $ if b then white else black
    p = rectangleSolid (s-1) (s-1)
  in t $ c p

ini :: (Float,Float) -> State -> State
ini (x',y') _ = listArray sRange $ randoms g where
  g = mkStdGen $ floor $ x'*y'

handle :: Event -> State -> State
handle (EventKey (MouseButton LeftButton) Up _ p) = ini p
handle _ = id

step :: Float -> State -> State
step f a = listArray sRange $ assocs a <&> \((x,y),b) -> let
    neighbors = do
      i <- [x-1..x+1]
      j <- [y-1..y+1]
      guard $ i /= x || j /= y
      guard $ inRange sRange (i,j)
      guard $ a ! (i,j)
    s = length neighbors
  in if b
    then s == 2 || s == 3
    else s == 3