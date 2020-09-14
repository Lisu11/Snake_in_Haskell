module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Snake

data Status = Won | Lost | Playing 
type Coordinates = (Float, Float)
data GameState = GameState {
        snake :: SnakeObject -- ^ snake status
    ,   points :: Int  -- ^ number of points
    ,   status :: Status -- ^ game status won, lost, playing
    ,   toNextBug :: Int -- ^ frames to next bug
    ,   bug  :: Coordinates  -- ^ coordinates of a bug
    ,   eatenBugs :: [Coordinates]
    } 

fps :: Int
fps = 20

window :: Display
window = FullScreen

bgColor :: Color
bgColor = black

pixel :: Float
pixel = 20

snakeColor :: Color
snakeColor = dark red

timeToNextBug :: Int
timeToNextBug = fps

initialState :: GameState
initialState = GameState {
        snake  = initialSnake
    ,   points = 0
    ,   status = Playing
    ,   toNextBug = timeToNextBug
    ,   bug  =  (-100, 0)
    ,   eatenBugs = []
    }

renderSegment :: Segment -> Picture
renderSegment (Seg Head (x, y)) = pictures 
    [ translate x y $ color snakeColor $ rectangleSolid pixel pixel
    ]
renderSegment (Seg Tail (x, y)) = pictures 
    [ translate x y $ color (light snakeColor) $ rectangleSolid pixel pixel
    ]
renderSegment (Seg Tip (x, y)) = pictures 
    [ translate x y $ color (light $ light snakeColor) $ rectangleSolid pixel pixel
    ]

render :: GameState -> Picture
render st = pictures (time : points : bug' : s)
    where
        (ss, _)  = snake st
        s        = map renderSegment ss
        (bx, by) = bug st
        bug'     = translate bx by $ color green $ circleSolid (pixel / 2)
        time     = scale 0.2 0.2 $ translate 4500 2500 $ color yellow $ (text . show . toNextBug) st
        points   = scale 0.25 0.25 $ translate (-3700) 2000 $ color red $ text $ show $ length $ fst $ snake st

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) st = st { snake = changeDir (snake st) U }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) st = st { snake = changeDir (snake st) D }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) st = st { snake = changeDir (snake st) R }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) st = st { snake = changeDir (snake st) L }
handleEvent _ st = st

bugEaten :: Coordinates -> SnakeObject -> Bool
bugEaten c1 snake@(Seg _ c2:_, _) = c1 == c2

onTip :: SnakeObject -> [Coordinates] -> Bool
onTip _ []         = False
onTip snake (c:cs) = 
    let (Seg _ c') = last (fst snake)
    in c' == c

-- headOnTopWall :: SnakeObject -> Bool
-- headOnTopWall (Seg _ (x, y):_, U) = 
-- headOnTopWall _ = False

-- headOnBotWall :: SnakeObject -> Bool
-- headOnLeftWall :: SnakeObject -> Bool
-- headOnRightWall :: SnakeObject -> Bool

update :: Float -> GameState -> GameState
update _ st@(GameState _ _ Won _ _ _) = st 
update _ st@(GameState _ _ Lost _ _ _) = st 
update _ st = st { snake     = snake'
                 , toNextBug = time
                 , bug       = bug' 
                 , eatenBugs = eatenBugs'
                 }
        where
            -- | bug got thru entire snake?
            shouldGrow = onTip (snake st) $ eatenBugs st
            snake' = moveForward pixel $ (if shouldGrow then grow  else id) $ snake st
            eatenBugs'' = (if shouldGrow then tail else id) $ eatenBugs st

            -- | new bug was eaten?
            eaten  = bugEaten (bug st) (snake st)
            eatenBugs' = if eaten then eatenBugs'' ++ [bug st] else eatenBugs''
            (time, bug') = if toNextBug st == 0 || eaten
                           then (timeToNextBug, (fromIntegral $ toNextBug st, 500))
                           else (toNextBug st - 1, bug st)


main :: IO ()
main = play window bgColor fps initialState render handleEvent update
