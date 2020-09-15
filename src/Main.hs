module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Snake
import System.Random
import Control.Monad.State

data Status = Won | Lost | Playing 
type Coordinates = (Float, Float)
data GameState = GameState {
        snake       :: SnakeObject -- ^ snake status
    ,   points      :: Int  -- ^ number of points
    ,   status      :: Status -- ^ game status won, lost, playing
    ,   toNextBug   :: Int -- ^ frames to next bug
    ,   bug         :: Coordinates  -- ^ coordinates of a bug
    ,   eatenBugs   :: [Coordinates]
    ,   stdGen      :: StdGen
    } 

fps :: Int
fps = 20

window :: Display
window = FullScreen
height, width :: Float
height = 1040
width  = 1920 

pixel :: Float
pixel = 20

hPixels :: Int
hPixels = floor (height / pixel)

bgColor :: Color
bgColor = black

snakeColor :: Color
snakeColor = dark red

timeToNextBug :: Int
timeToNextBug = 5 * fps

initialState :: GameState
initialState = GameState {
        snake       = initialSnake
    ,   points      = 0
    ,   status      = Playing
    ,   toNextBug   = timeToNextBug
    ,   bug         = (-100, 0)
    ,   eatenBugs   = []
    ,   stdGen      = mkStdGen 0
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
render st = pictures (p2 : p : time : points : bug' : s)
    where
        (ss, _)  = snake st
        s        = map renderSegment ss
        (bx, by) = bug st
        bug'     = translate bx by $ color green $ circleSolid (pixel / 2)
        time     = scale 0.2 0.2 $ translate 4500 2500 $ color yellow $ text $ show  (toNextBug st)
        points   = scale 0.25 0.25 $ translate (-3700) 2000 $ color red $ text $ show $ 
                    ((length $ fst $ snake st) - (length $ fst $ initialSnake))
        p = color red $ text $ show (bug st)
        p2= translate 0 (-100) $ color green $ text $ show $ getHeadCoordinates $ snake st
         

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) st = st { snake = changeDir (snake st) U }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) st = st { snake = changeDir (snake st) D }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) st = st { snake = changeDir (snake st) R }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) st = st { snake = changeDir (snake st) L }
handleEvent _ st = st

bugEaten :: Coordinates -> SnakeObject -> Bool
bugEaten c1 snake = c1 == getHeadCoordinates snake

onTip :: SnakeObject -> [Coordinates] -> Bool
onTip _ []         = False
onTip snake (c:cs) =  c == getTipCoordinates snake 
    

headOnTopWall :: SnakeObject -> Bool
headOnTopWall s@(_, U) = y + pixel / 2 >= height / 2
    where 
        (_, y) = getHeadCoordinates s
headOnTopWall _ = False

headOnBotWall :: SnakeObject -> Bool
headOnBotWall s@(_, D) = y - pixel / 2 <= (- height) / 2
    where 
        (_, y) = getHeadCoordinates s
headOnBotWall _ = False

headOnLeftWall :: SnakeObject -> Bool
headOnLeftWall s@(_, L) = x - pixel / 2 <= (- width) / 2
    where 
        (x, _) = getHeadCoordinates s
headOnLeftWall _ = False

headOnRightWall :: SnakeObject -> Bool
headOnRightWall s@(_, R) = x + pixel / 2 >= width / 2
    where 
        (x, _) = getHeadCoordinates s
headOnRightWall _ = False

moveThruWallIfNesesery :: SnakeObject -> SnakeObject
moveThruWallIfNesesery snake 
    | headOnTopWall snake   = moveForward (- height) snake
    | headOnBotWall snake   = moveForward (- height) snake
    | headOnLeftWall snake  = moveForward (- width) snake
    | headOnRightWall snake = moveForward (- width) snake
    | otherwise             = moveForward pixel snake

update :: Float -> GameState -> GameState
update _ st@(GameState _ _ Won _ _ _ _) = st 
update _ st@(GameState _ _ Lost _ _ _ _) = st 
update _ st = st { snake     = snake'
                 , toNextBug = time
                 , bug       = bug' 
                 , eatenBugs = eatenBugs'
                 , stdGen    = stdGen'
                 }
        where
            -- | bug got thru entire snake?
            shouldGrow = onTip (snake st) $ eatenBugs st
            snake' = moveThruWallIfNesesery $ (if shouldGrow then grow  else id) $ snake st
            eatenBugs'' = (if shouldGrow then tail else id) $ eatenBugs st

            -- | new bug was eaten?
            eaten  = bugEaten (bug st) (snake st)
            eatenBugs' = if eaten then eatenBugs'' ++ [bug st] else eatenBugs''
            (time, bug', stdGen') = 
                if toNextBug st == 0 || eaten
                then let (s, x, y) = genRandomPosition $ stdGen st
                     in (timeToNextBug, (x, y), s)
                else (toNextBug st - 1, bug st, stdGen st)


genRandomPosition :: StdGen -> (StdGen, Float, Float)
genRandomPosition gen = 
    let 
        (x, s') = randomR ((-width / 2), width / 2) gen
        (y, s ) = randomR ((-height / 2), height / 2) s'
        roundX  = fromIntegral $ toTen (floor x) 
        roundY  = fromIntegral $ toTen (floor y) 
        toTen :: Int -> Int
        toTen v = v - (rem v 20)
    
    in (s, roundX, roundY)

main :: IO ()
main = play window bgColor fps initialState render handleEvent update
