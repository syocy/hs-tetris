module Game where

import Input

class Game g where
  update :: [TInput] -> g -> g
  render :: g -> IO ()
  isGameover :: g -> Bool
  
