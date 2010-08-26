import Game
import Input
import DataType
import Util

fieldHeight = 20
fieldWidth  = 10

--data Tetris = Tetris (GameVariables,[GameObject])
data Tetris = Tetris {
  gameVariables :: GameVariables,
  gameObjects :: [GameObject]
}
data GameVariables = GameVariables {
  flagGameover :: Bool 
}
data GameObject =
  TetrominoObj {
    position :: Cartesian,
    content :: Tetromino Cartesian,
    degree :: Int
  } |
  PileObj {
    content :: Tetromino Cartesian
  }
  
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
    
-- collidep :: [GameObject] -> Tetromino Cartesian -> Bool
-- collidep objs t = any collidep' objs
--   where collidep' :: GameObject -> Bool
--         collidep' (PileObj {coorXs=}) = 

instance Game Tetris where
  update = updateTetris
  render = renderTetris
  isGameover = isTetrisOver

updateTetris :: [TInput] -> Tetris -> Tetris
updateTetris = undefined
renderTetris :: Tetris -> IO ()
renderTetris = undefined
isTetrisOver :: Tetris -> Bool
isTetrisOver = undefined
