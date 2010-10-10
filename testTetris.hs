import Tetris
import DataType
import Util
import Game
import Input

import System.Posix.Unistd(usleep)
import Data.Maybe(fromJust)

t = TetrominoObj {
      position = Cartesian 2 3
--    , content = last tetromino
--    , content = head tetromino
--    , content = head $ tail tetromino
    , content = head $ tail $ tail tetromino
--    , degree = 3
    }

p = PileObj {
      content = maketet [(0, 14), (1, 14), (2, 14), (3, 14), (4, 14),
                        (5, 14), (6, 14), (7, 14), (8, 14), (9, 14)]
      -- content = Tetromino [Cartesian 0 9, Cartesian 1 9, Cartesian 2 9,
      --                     Cartesian 1 8, Cartesian 2 8]
    }

go = p : t : []

gv = GameVariables 1 [1,2..]

tetris = Tetris gv go

main = do
  render tetris
  render $ Tetris (gameVariables tetris) (deletePile (gameObjects tetris))
--  render $ Tetris (gameVariables tetris) (rawDeletePile 14 (gameObjects tetris))

-- main = do
--   initBufferingMode
--   render tetris
--   loop tetris
--   where
--     loop tetris = do
--          usleep 1000000
--          x <- input initialTInput
--          print x
-- --         let nt = Tetris (gameVariables tetris) (move (fromJust x) (gameObjects tetris))
--          let nt = Tetris (gameVariables tetris) (fall $ gameObjects tetris)
--          render nt
--          loop nt
  