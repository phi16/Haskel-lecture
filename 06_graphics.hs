import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play (InWindow "po" (400,400) (400,400)) black 60 iniWorld render handle step

newtype State = State (Bool,Float)

iniWorld :: State
iniWorld = State (True,-100)

render :: State -> Picture
render (State (_,d)) = pictures [
  color red $ rectangleSolid 60.0 60.0,
  color yellow $ translate d 0 $ circleSolid 20.0 ]

handle :: Event -> State -> State
handle (EventKey (MouseButton LeftButton) Down _ _) (State (e,d)) = State (not e,d)
handle _ s = s

step :: Float -> State -> State
step f (State (e,d)) = State $ (e,d + f * e' * 30.0) where
  e' = if e then 1 else -1