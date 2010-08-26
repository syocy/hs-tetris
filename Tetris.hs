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
    position :: Cartesian,
    content :: Tetromino Cartesian,
    degree :: Int
  } |
  PileObj {
    content :: Tetromino Cartesian
  } deriving(Show,Eq)
  
deletePile :: [GameObject] -> Int -> [GameObject]
deletePile objs y = map close objs
  where
    close :: GameObject -> GameObject
    close (PileObj p) = PileObj . Tetromino $ filter hitp $ blocks p
    close x = x 
    hitp :: Cartesian -> Bool
    hitp (Cartesian _ y') = y == y'
    
transform :: GameObject -> Tetromino Cartesian
transform (TetrominoObj {position=p, content=t, degree=d}) =
  ptrans p $ rotaten t d
transform x = content x
    
collidep :: [GameObject] -> Tetromino Cartesian -> Bool
collidep objs t = any collidep' objs
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

updateTetris :: [TInput] -> Tetris -> Tetris
updateTetris = undefined

renderTetris :: Tetris -> IO ()
renderTetris (Tetris gv go) = putStr $ stringForRender go
    
isTetrisOver :: Tetris -> Bool
isTetrisOver (Tetris gv go) = any over go
  where
    over :: GameObject -> Bool
    over (PileObj p) = 0 > (minimum $ toYs p)
    over _ = False
