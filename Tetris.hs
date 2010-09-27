module Tetris where

import Game
import Input
import DataType
import Util

import Data.List(intersect,sort)

fieldHeight = 20
fieldWidth  = 10

--data Tetris = Tetris (GameVariables,[GameObject])
data Tetris = Tetris {
  gameVariables :: GameVariables,
  gameObjects :: [GameObject]
}
data GameVariables = GameVariables {
  level :: Int
}
data GameObject =
  TetrominoObj {
      position :: Cartesian
    , content :: Tetromino Cartesian
--    , degree :: Int
  } |
  PileObj {
    content :: Tetromino Cartesian
  } deriving(Show,Eq)
  
deletePile :: Int -> [GameObject] -> [GameObject]
deletePile y = map close
  where
    close :: GameObject -> GameObject
    close (PileObj p) = PileObj . Tetromino $ filter noHitp $ blocks p
    close x = x
    noHitp :: Cartesian -> Bool
    noHitp (Cartesian _ y') = not $ y == y'
    
-- transform :: GameObject -> Tetromino Cartesian
-- transform (TetrominoObj {position=p, content=t, degree=d}) =
--   ptrans p $ rotaten t d
-- transform x = content x
    
fieldOverp :: Tetromino Cartesian -> Bool
fieldOverp = any fieldOverp' . toPairs
  where fieldOverp' :: (Int,Int) -> Bool
        fieldOverp' (x,y)
          | x < 0 || x >= fieldWidth || y < 0 || y >= fieldHeight = True
          | otherwise = False
    
collidep :: Tetromino Cartesian -> [GameObject] -> Bool
collidep t = fieldOverp t || any collidep'
  where collidep' :: GameObject -> Bool
        collidep' (PileObj p) = not . null $ blocks t `intersect` blocks p
        collidep' _ = False
        
allCoors :: [GameObject] -> [Cartesian]
allCoors (x:xs) = (blocks $ transform x) ++ allCoors xs
allCoors [] = []

stringForRender :: [GameObject] -> String
stringForRender = 
  flip (++) underline . stringForRender' 0 . sort . allCoors
  where
    underline :: String
    underline = replicate (fieldWidth+2) '-' ++ "\n"
    stringForRender' :: Int -> [Cartesian] -> String
    stringForRender' _ [] = []
    stringForRender' n xs = toString upper ++ stringForRender' (n+1) lower
      where
        (upper,lower) = span (<= Cartesian fieldWidth n) xs
        toString :: [Cartesian] -> String
        toString ys = '|' : toString' (-1) ys ++ "|\n"
        toString' :: Int -> [Cartesian] -> String
        toString' m [] = replicate (fieldWidth-m-1) ' '
        toString' m ((Cartesian z _):zs) =
          replicate (z-m-1) ' ' ++ "*" ++ toString' z zs

instance Game Tetris where
  update = updateTetris
  render = renderTetris
  isGameover = isTetrisOver

updateTetris :: TInput -> Tetris -> Tetris
updateTetris = undefined
-- updateTetris tinput (Tetris gv go) =Tetris gv $ map (updateTetris' tinput) go
--   where
--     updateTetris' :: TInput -> GameObject -> GameObject
--     updateTetris' = undefined
--     newPos :: TInput -> Tetromino Cartesian -> Tetromino Cartesian
--     newPos TLeft pos = pos + if x pos == 0 then 0 else Cartesian -1 0
--     newPos TRight pos = pos + if x pos == fieldWidth-1 then 0 else Cartesian 1 0
--     newPos TDown pos = undefined
--     newPos TSpace 
    -- updateTetris' TLeft (TetrominoObj pos cont deg) =
    --   let pos' = if x pos == 0 then pos else pos + Cartesian -1 0 in
    --   TetrominoObj pos' cont deg
    -- updateTetris' TRight (TetrominoObj pos cont deg) =
    --   let pos' = if x pos == fieldWidth-1 then pos else pos + Cartesian 1 0 in
    --   TetrominoObj pos' cont deg
    -- updateTetris' TDown (TetrominoObj pos cont deg) =
    --   let pos' = if collidep go $ transform 

renderTetris :: Tetris -> IO ()
renderTetris (Tetris gv go) = putStr $ stringForRender go
    
isTetrisOver :: Tetris -> Bool
isTetrisOver (Tetris gv go) = any over go
  where
    over :: GameObject -> Bool
    over (PileObj p) = 0 > (minimum $ toYs p)
    over _ = False
