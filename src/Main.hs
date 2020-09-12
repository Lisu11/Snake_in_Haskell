module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Snake

data Status = Won | Lost | Playing 

data GameState = GameState {
        snake :: SnakeObject -- ^ snake status
    ,   points :: Int  -- ^ number of points
    ,   status :: Status -- ^ game status won, lost, playing
    ,   toNextBug :: Int -- ^ frames to next bug
    ,   bugCoord  :: (Float, Float)  -- ^ coordinates of a bug
    }

fps :: Int
fps = 60

window :: Display
window = FullScreen

bgColor :: Color
bgColor = black

pixel :: Float
pixel = 20

snakeColor :: Color
snakeColor = dark red

initialState :: GameState
initialState = GameState {
        snake  = initialSnake
    ,   points = 0
    ,   status = Playing
    ,   toNextBug = 1000
    ,   bugCoord  =  (-100, 200)
    }

renderSegment :: Segment -> Picture
renderSegment (Head x y) = pictures 
    [ translate x y $ color snakeColor $ rectangleSolid pixel pixel
    ]
renderSegment (Tail x y) = pictures 
    [ translate x y $ color (light snakeColor) $ rectangleSolid pixel pixel
    ]
renderSegment (Tip x y) = pictures 
    [ translate x y $ color (light $ light snakeColor) $ rectangleSolid pixel pixel
    ]

bugEaten :: GameState -> Bool
bugEaten st = 
    let (Head x y) = head $ fst (snake st)
        (bx, by)   = bugCoord st
    in bx == x && by == y

render :: GameState -> Picture
render st = pictures (bug : s)
    where
        (ss, _)  = snake st
        s        = map renderSegment ss
        (bx, by) = bugCoord st
        bug      = translate bx by $ color green $ circleSolid pixel

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) st = st { snake = changeDir (snake st) U }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) st = st { snake = changeDir (snake st) D }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) st = st { snake = changeDir (snake st) R }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) st = st { snake = changeDir (snake st) L }
handleEvent _ st = st

update :: Float -> GameState -> GameState
update _ st@(GameState _ _ Won _ _) = st 
update _ st@(GameState _ _ Lost _ _) = st 
update _ st = st { snake     = moveForward  (snake st) (pixel / 3)
                 , toNextBug = time
                 , bugCoord  = b 
                 }
        where
            snake' = moveForward  (snake st) (pixel / 3)
            eaten = bugEaten st {snake = snake'}
            snake''   = if eaten then eat (bugCoord st) snake' else snake'
            (time, b) = if toNextBug st - 1 == 0 || eaten
                        then (9000, (500, 500))
                        else (toNextBug st - 1, bugCoord st)


main :: IO ()
main = play window bgColor fps initialState render handleEvent update
