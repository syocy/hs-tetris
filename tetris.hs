import Game
import Input
import DataType
import Util

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
    content :: Tetromino Cartesian,
    howModern :: Int
  }
  
deletePile :: [GameObject] -> Int -> [GameObject]
deletePile obj n = map close $ filter upperp obj
  where
    upperp (PileObj {howModern=x}) = not . (==) n $ x
    upperp _ = True
    close (PileObj {content=c, howModern=m}) 
      | (m >= n)  = PileObj { content = c, howModern = m-1 }
      | otherwise = PileObj { content = c, howModern = m }
    close x = x
    
transform :: GameObject -> Tetromino Cartesian
transform (TetrominoObj {position=p, content=t, degree=d}) =
  ptrans p $ rotaten t d
    
-- collidep :: [GameObject] -> TetrominoObj -> Bool
-- collidep obj t = any collidep' obj
--   where collidep' :: GameObject -> Bool
--         collidep' 

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
