module DataType where

import Control.Monad(join)

-- 2次元座標の型
data Cartesian = Cartesian {
  x :: Int,
  y :: Int
} deriving (Eq,Show)

-- テトリスブロックの型
data Tetromino a = Tetromino {
  blocks :: [a]
} deriving (Eq,Show)

--type Tetromino' = Tetromino Cartesian

--newtype Tetromino a = Tetromino [a]
--                    deriving (Eq,Show)

-- 平行移動のための (+)
instance Num Cartesian where
  (+) a b = Cartesian ((x a) + (x b)) ((y a) + (y b))
  (*) a b = Cartesian ((x a) * (x b)) ((y a) * (y b))
  negate a = Cartesian (negate (x a)) (negate (y a))
  abs a = Cartesian posx posy
    where posx = if (x a) >= 0 then (x a) else negate (y a)
          posy = if (y a) >= 0 then (y a) else negate (y a)
  signum a
    | (x a) * (y a) > 0 = Cartesian 1 1
    | (x a) * (y a) < 0 = Cartesian (-1) (-1)
    | otherwise = Cartesian 0 0
  fromInteger integer = Cartesian int int
    where int = fromInteger integer

-- Tetromino を文字列表示するため
instance Ord Cartesian where
  (<=) a b
    | (y a) < (y b) = True
    | (y a) == (y b) && (x a) <= (x b) = True
    | otherwise = False

-- Tetromino の map を簡単にする
instance Functor Tetromino where
  fmap f (Tetromino t) = Tetromino . map f $ t
  
instance Monad Tetromino where
  return x = Tetromino [x]
--  t >>= f = join $ fmap f t
  (>>=) t = join . flip fmap t
--  (Tetromino t) >>= f = Tetromino . (=<<) (blocks . f) $ t -- (t >>= blocks . f)
--  (Tetromino t) >>= f = Tetromino . concat . map (blocks . f) $ t
  
--newtype Pile a = Pile (Tetromino a)