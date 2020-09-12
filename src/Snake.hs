module Snake(Segment(..), SnakeObject, initialSnake, moveForward, Direction(..), changeDir, eat) where
import Control.Monad.State


data Direction = U | D | R | L deriving Eq

type SnakeObject = ([Segment], Direction)

data Segment = Head Float Float | Tail Float Float | Tip Float Float 

updateX :: Float -> Segment -> Segment
updateX dx (Tail x y) = Tail (dx + x) y
updateX dx (Head x y) = Head (dx + x) y
updateX dx (Tip x y) = Tip (dx + x) y

updateY :: Float -> Segment -> Segment
updateY dy (Tail x y) = Tail x (dy + y)
updateY dy (Head x y) = Head x (dy + y)
updateY dy (Tip x y) = Tip x (dy + y)

mv :: (Float -> Segment -> Segment) -> Float -> Segment -> State (Float, Float) Segment
mv f d s@(Head x y)= do
    put (x, y)
    return (f d s)
mv _ _ (Tail x y) = do
    (x', y') <- get
    put (x, y)
    return (Tail x' y')
mv _ _ (Tip x y) = do
    (x', y') <- get
    put (x, y)
    return (Tip x' y')

mapState' :: (Float, Float) -> [State (Float, Float) Segment] -> [Segment] 
mapState' _ []     = [] 
mapState' xy (h:t) = let (v, s) = runState h xy
                    in v : mapState' s t


moveForward :: SnakeObject -> Float -> SnakeObject
moveForward (snake, R) d = (mapState' (0,0) $ map (mv updateX d) snake, R)
moveForward (snake, L) d = (mapState' (0,0) $ map (mv updateX (-d)) snake, L)
moveForward (snake, U) d = (mapState' (0,0) $ map (mv updateY d) snake, U)
moveForward (snake, D) d = (mapState' (0,0) $ map (mv updateY (-d)) snake, D)

changeDir :: SnakeObject -> Direction -> SnakeObject
changeDir s@(snake, D) U = s
changeDir s@(snake, U) D = s
changeDir s@(snake, R) L = s
changeDir s@(snake, L) R = s
changeDir s@(snake, d1) d2 
    | d1 == d2  = s
    | otherwise = (snake, d2)

eat :: (Float, Float) -> SnakeObject -> SnakeObject
eat (x, y) ((Head x' y'):t, d) = 
    let h' = Head x y
        h''= Tail x' y'
    in    (h':h'':t, d)
eat _ s = error "Snake wrongly formatted"
    

initialSnake :: SnakeObject
initialSnake = ([Head 0 0, Tail (-20) 0, Tip (-40) 0], R)