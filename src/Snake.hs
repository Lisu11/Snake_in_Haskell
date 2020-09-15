module Snake(Segment(..),SegmentType(..), SnakeObject, initialSnake, moveForward, Direction(..), changeDir, grow,getHeadCoordinates,getTipCoordinates, checkCollisionWithItself, snakeLen) where
import Control.Monad.State


data Direction = U | D | R | L deriving Eq

type SnakeObject = ([Segment], Direction)

data SegmentType = Head | Tail | Tip

data Segment = Seg SegmentType (Float,  Float) 

instance Eq Segment where
    (Seg _ c1) == (Seg _ c2) = c1 == c2

moveForward :: Float -> SnakeObject -> SnakeObject
moveForward d snake@(Seg _ (x, y) :_, R) = 
    fst $ runState (move snake) (x+d, y)
moveForward d snake@(Seg _ (x, y) :_, L) = 
    fst $ runState (move snake) (x-d, y)
moveForward d snake@(Seg _ (x, y) :_, U) = 
    fst $ runState (move snake) (x, y+d)
moveForward d snake@(Seg _ (x, y) :_, D) = 
    fst $ runState (move snake) (x, y-d)
    


move :: SnakeObject -> State (Float, Float) SnakeObject
move ([], dir) = return ([], dir)
move ((Seg typ (x, y)):t, dir) = do
    (x', y') <- get
    put (x, y)
    (rest, _) <- move (t, dir)
    return ((Seg typ (x', y')):rest, dir)


changeDir :: SnakeObject -> Direction -> SnakeObject
changeDir s@(snake, D) U = s
changeDir s@(snake, U) D = s
changeDir s@(snake, R) L = s
changeDir s@(snake, L) R = s
changeDir s@(snake, d1) d2 
    | d1 == d2  = s
    | otherwise = (snake, d2)



grow :: SnakeObject -> SnakeObject 
grow ((Seg Tip cs):ss, dir) = (Seg Tail cs : Seg Tip cs : ss, dir)
grow (h:t, dir)             = let (s, _) = grow (t, dir)
                              in (h:s, dir)  

getHeadCoordinates :: SnakeObject -> (Float, Float)
getHeadCoordinates (Seg _ c:_, _) = c   

getTipCoordinates :: SnakeObject -> (Float, Float)
getTipCoordinates (s, _) = let (Seg _ c) = last s
                            in c

checkCollisionWithItself :: SnakeObject -> Bool
checkCollisionWithItself (h:t, _) = not $ all (h /=) t
    
snakeLen :: SnakeObject -> Int
snakeLen = length . fst

initialSnake :: SnakeObject
initialSnake = ([Seg Head (0, 0), Seg Tail ((-20), 0), Seg Tip ((-40), 0)], R)