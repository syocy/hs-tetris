import Tetris
import DataType
import Util
import Game

t = TetrominoObj {
      position = Cartesian 1 1,
      content = head $ tail $ tail tetromino,
      degree = 3
    }

p = PileObj {
      content = Tetromino [Cartesian 0 9, Cartesian 1 9, Cartesian 2 9,
                          Cartesian 1 8, Cartesian 2 8]
    }

go = p : t : []

gv = GameVariables 1

tetris = Tetris gv go

