import Graphics.Gloss

main :: IO ()
main = display (InWindow "po" (400,400) (400,400)) black pic

pic :: Picture
pic = pictures [ color red $ rectangleSolid 60.0 60.0, color yellow $ circleSolid 20.0 ]