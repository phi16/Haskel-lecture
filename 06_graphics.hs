import Graphics.Gloss

main :: IO ()
main = animate (InWindow "po" (400,400) (400,400)) black pic

pic :: Float -> Picture
pic t = pictures [ color red $ rectangleSolid 60.0 60.0, 
                   color yellow $ translate (t*10-100) 0 $ circleSolid 20.0 ]