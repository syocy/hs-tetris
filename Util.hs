module Util where

import DataType
import Input
import Data.List(sort)    

-- 整数のペアのリストからテトリスブロックを作る関数
maketet :: [(Int,Int)] -> Tetromino Cartesian
maketet = Tetromino . map makecart
  where
    makecart :: (Int,Int) -> Cartesian
    makecart x = Cartesian (fst x) (snd x)

-- maketet の逆
toPairs :: Tetromino Cartesian -> [(Int, Int)]
toPairs = map toPair . blocks
    where toPair :: Cartesian -> (Int, Int)
          toPair (Cartesian x y) = (x, y)

toXs :: Tetromino Cartesian -> [Int]
toXs = map x . blocks

toYs :: Tetromino Cartesian -> [Int]
toYs = map y . blocks
          
tetromino =
 [
    maketet [(0,0),(1,0),(0,1),(1,1)] -- O
  , maketet [(0,0),(1,0),(2,0),(3,0)] -- I
  , maketet [(0,0),(1,0),(1,1),(2,1)] -- Z
  , maketet [(0,1),(1,1),(1,0),(2,0)] -- S
  , maketet [(0,0),(1,0),(2,0),(2,1)] -- J
  , maketet [(2,0),(1,0),(0,0),(0,1)] -- L
  , maketet [(1,0),(0,1),(1,1),(2,1)] -- T
 ]

-- 平行移動関数
ptrans :: Cartesian -> Tetromino Cartesian -> Tetromino Cartesian
ptrans p = fmap (+p)
-- ptrans p = bind (+ p)

-- 反時計90度の単純回転関数
simpleRotate :: Tetromino Cartesian -> Tetromino Cartesian
simpleRotate = fmap mat
  where
    mat :: Cartesian -> Cartesian
    mat c = Cartesian (y c) (negate (x c))
        
-- 中心座標での回転関数
rotate :: Tetromino Cartesian -> Tetromino Cartesian
-- rotate t = ptrans halfc . simpleRotate . ptrans (negate halfc) $ t
--   where
--     halfc :: Cartesian
-- --    halfc = head $ tail $ blocks t
rotate t = let rotated = simpleRotate $ ptrans (Cartesian (-1) (-1)) t in
  let minx = minimum . toXs $ rotated in
  let miny = minimum . toYs $ rotated in
  case (minx, miny) of
    (x, y) | x<0 && y<0 -> ptrans (Cartesian (-x) (-y)) rotated
           | x<0 -> ptrans (Cartesian (-x) 0) rotated
           | y<0 -> ptrans (Cartesian 0 (-y)) rotated
           | otherwise -> rotated
            
times :: Int -> (a -> a) -> a -> a
times n = foldl (.) id . replicate n

rotaten :: Int -> Tetromino Cartesian -> Tetromino Cartesian
rotaten n = times n rotate

-- Tetromino を文字列に(デバッグ用?)
showOne :: Tetromino Cartesian -> String
showOne = showOne' (Cartesian 0 0) . sort . blocks . ptrans (Cartesian 2 2)
showOne' a (b:bs)
  | a == b  = '*' : showOne' right bs
  | not $ (y a) == (y b) = '\n' : showOne' newline (b:bs)
  | otherwise = ' ' : showOne' right (b:bs)
    where
      right = a + (Cartesian 1 0)
      newline = Cartesian 0 ((y a) + 1)
showOne' _ [] = []

-- showOne を ghci で見るため
putOne :: Tetromino Cartesian -> IO ()
putOne = putStrLn . showOne

-- times :: a -> (a -> a) -> Int -> a
-- times x _ 0 = x
-- times x f n = times (f x) f (n-1)
