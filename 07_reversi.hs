{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

import Graphics.Gloss hiding (Blank)
import Graphics.Gloss.Interface.IO.Game hiding (Blank)
import Data.Array
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State.Lazy hiding (State)
import Control.Lens hiding (rewrite)
import System.Random

w :: Int
w = 500

data Cell = Mine | Yours | Blank
  deriving (Eq,Show)
type State = Array (Int,Int) Cell

data Mode = MyTurn | YourTurn | Result Cell
  deriving Show
data World = World {
  _mode :: Mode,
  _field :: State
}
makeLenses ''World

sRange :: ((Int,Int),(Int,Int))
sRange = ((0,0),(7,7))

iniField :: State
iniField = blanks // [((3,3),Mine),((3,4),Yours),((4,3),Yours),((4,4),Mine)] where
  blanks = listArray sRange $ repeat Blank

iniWorld :: World
iniWorld = World {
  _mode = MyTurn,
  _field = iniField
}

render :: World -> IO Picture
render a = return $ pictures $ execWriter $ renderElem a

renderElem :: World -> Writer [Picture] ()
renderElem a = do
  let
    w' = fromIntegral w
    draw x = tell [x]
  forM_ (assocs $ a^.field) $ \((x',y'),c) -> do
    let
      x = fromIntegral x' + 0.5
      y = fromIntegral y' + 0.5
      s = fromIntegral w / 9
      t = translate (x*s-w'/2+s/2) (y*s-w'/2+s/2)
      b = color (dark green) $ rectangleSolid (s-1) (s-1)
      p = case c of
        Mine -> color black $ circleSolid $ s/2
        Yours -> color white $ circleSolid $ s/2
        Blank -> blank
    draw $ t $ pictures [b, p]
  draw $ color white $ translate (-w'/2) (w'/2-20) $ scale 0.17 0.17 $ text $ show $ a^.mode

handle :: Event -> World -> IO World
handle (EventKey (MouseButton LeftButton) Down _ (x',y')) wor = execStateT ?? wor $ do
  let
    w' = fromIntegral w
    s = w' / 9
    x = floor $ (x'+w'/2)/s-0.5
    y = floor $ (y'+w'/2)/s-0.5
    dirs = [(x,y)|x<-[-1..1],y<-[-1..1],x/=0||y/=0]
    see :: Cell -> (Int,Int) -> Int -> StateT State IO Int
    see c (i,j) cnt = do
      let p = (x+i*(cnt+1),y+j*(cnt+1))
      m <- preuse $ ix p
      case m of
        Nothing -> return 0
        Just Blank -> return 0
        Just t
          | t == c -> return cnt
          | otherwise -> do
            d <- preuse $ ix p
            ix p .= c
            u <- see c (i,j) $ cnt+1
            when (u == 0) $ case d of
              Just d' -> ix p .= d'
              _ -> return ()
            return u
    turn :: Mode -> StateT World IO ()
    turn tu = do
      let
        c = case tu of
          MyTurn -> Mine
          YourTurn -> Yours
          _ -> undefined
      s <- zoom field $ fmap sum $ forM dirs $ \(i,j) -> see c (i,j) 0
      when (s/=0) $ do
        field.ix (x,y) .= c
        mode .= case tu of
          MyTurn -> YourTurn
          YourTurn -> MyTurn
          x -> x
  m <- use mode
  avail <- preuse $ field.ix (x,y)
  case avail of
    Just Blank -> case m of
      Result _ -> return ()
      x -> turn x
    _ -> return ()
handle _ x = return x

step :: Float -> World -> IO World
step f = return

main :: IO ()
main = playIO (InWindow "Reversi" (w,w) (w,w)) blue 60 iniWorld render handle step
