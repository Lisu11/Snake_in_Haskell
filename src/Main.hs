module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Snake
import System.Random
import Control.Monad.State

data Status = Menu Float | Lost | Playing 
type Coordinates = (Float, Float)
data GameState = GameState {
        status      :: Status       -- ^ game status won, lost, playing
    ,   snake       :: SnakeObject  -- ^ snake status
    ,   points      :: Int          -- ^ number of points
    ,   toNextBug   :: Int          -- ^ frames to next bug
    ,   bug         :: Coordinates  -- ^ coordinates of a bug
    ,   eatenBugs   :: [Coordinates]
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
    ,   status      = Menu 0
    ,   toNextBug   = timeToNextBug
    ,   bug         = (-100, 0)
    ,   eatenBugs   = []
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
render st@(GameState (Menu item) _ _ _ _ _) = pictures [title, selectedItem, play, exit, author]
    where
        title        = translate (-800) 50 $ color (dark yellow) $ text "Snake Game"
        selectedItem = translate (-140) (-40 - (60 * item)) $ color red $ rectangleWire 200 60
        play         = scale 0.4 0.4 $ translate (-500) (-150) $ color red $ text $ "PLAY" 
        exit         = scale 0.4 0.4 $ translate (-500) (-300) $ color red $ text $ "EXIT" 
        author       = scale 0.2 0.2 $ translate (-500) (-2200) $ color green $ text $ "Author: Lisu"
render st@(GameState Lost _ _ _ _ _) = pictures (lostMsg : score : bindings : s)
    where 
        s       = map renderSegment (fst $ snake st)
        lostMsg = translate (-300) 50 $ color (light red) $ text "You Lost"
        score   = scale 0.4 0.4 $ translate (-500) (-100) $ color red $ text $ "Final Score: " ++ points
        points  = show $ ((snakeLen $ snake st) - (snakeLen $ initialSnake))
        bindings= scale 0.4 0.4 $ translate (-1300) (-400) $ color (dark $ dark red) $ text $ "Press ENTER to play again or ESC to exit"
render st = pictures (time : points : bug' : s)
    where
        (ss, _)  = snake st
        s        = map renderSegment ss
        (bx, by) = bug st
        bug'     = translate bx by $ color green $ circleSolid (pixel / 2)
        time     = scale 0.2 0.2 $ translate 4500 2500 $ color yellow $ text $ show  (toNextBug st)
        points   = scale 0.25 0.25 $ translate (-3700) 2000 $ color red $ text $ show $ 
                    ((snakeLen $ snake st) - (snakeLen $ initialSnake))

         

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) st@(GameState (Menu i) _ _ _ _ _) = 
    st { status = Menu $ fromIntegral (mod (floor i + 1) 2) }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) st@(GameState (Menu i) _ _ _ _ _) = 
    st { status = Menu $ fromIntegral (mod (floor i + 1) 2) }
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) st@(GameState (Menu i) _ _ _ _ _) = 
    if i == 0 then st {status = Playing} else error "Exit app"
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) st@(GameState Lost _ _ _ _ _) = 
    initialState { status = Playing }  
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) st = 
    st { snake = changeDir (snake st) U }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) st = 
    st { snake = changeDir (snake st) D }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) st = 
    st { snake = changeDir (snake st) R }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) st = 
    st { snake = changeDir (snake st) L }
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
update _ st@(GameState (Menu _) _ _ _ _ _) = st 
update _ st@(GameState Lost _ _ _ _ _) = st 
update _ st = st { snake     = snake'
                 , toNextBug = time
                 , bug       = bug' 
                 , eatenBugs = eatenBugs'
                 , status    = status'
                 }
        where
            -- | bug got thru entire snake?
            shouldGrow = onTip (snake st) $ eatenBugs st
            snake' = moveThruWallIfNesesery $ (if shouldGrow then grow  else id) $ snake st
            eatenBugs'' = (if shouldGrow then tail else id) $ eatenBugs st

            -- | new bug was eaten?
            eaten  = bugEaten (bug st) (snake st)
            eatenBugs' = if eaten then eatenBugs'' ++ [bug st] else eatenBugs''
            (time, bug') = 
                if toNextBug st == 0 || eaten
                then let s1 = snakeLen snake'
                         s2 = floor $ fst $ getHeadCoordinates snake'
                         s3 = floor $ snd $ getHeadCoordinates snake'
                         s4 = toNextBug st
                         seed =  s1 * s4 + s2 - s3 
                     in(timeToNextBug, genRandomPosition $ mkStdGen seed)
            else (toNextBug st - 1, bug st)
                    
                            
            
            -- | maybe you have lost?
            status' = if checkCollisionWithItself $ snake' then Lost else Playing



genRandomPosition :: StdGen -> (Float, Float)
genRandomPosition gen = (roundX, roundY)
    where 
        (x, s) = randomR ((-width / 2), width / 2) gen
        (y, _) = randomR ((-height / 2), height / 2) s
        roundX  = fromIntegral $ toTen (floor x) 
        roundY  = fromIntegral $ toTen (floor y) 
        toTen :: Int -> Int
        toTen v = v - (rem v 20)

main :: IO ()
main = play window bgColor fps initialState render handleEvent update
