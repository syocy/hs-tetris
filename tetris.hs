--import Data.Maybe
import Control.Monad

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

instance Ord Cartesian where
  (<=) a b
    | (y a) < (y b) = True
    | (y a) == (y b) && (x a) <= (x b) = True
    | otherwise = False

instance Functor Tetromino where
  fmap f (Tetromino t) = Tetromino . map f $ t
  
--(>>?) :: Tetromino a -> (a -> Tetromino b) -> Tetromino b
--(Tetromino t) >>? f = Tetromino . concat . map (blocks . f) $ t
--  where blocks (Tetromino x) = x
  
instance Monad Tetromino where
  return x = Tetromino [x]
--  t >>= f = join $ fmap f t
  (>>=) t = join . flip fmap t
--  (Tetromino t) >>= f = Tetromino . (=<<) (blocks . f) $ t -- (t >>= blocks . f)
--  (Tetromino t) >>= f = Tetromino . concat . map (blocks . f) $ t

-- 整数のペアのリストからテトリスブロックを作る関数
maketet :: [(Int,Int)] -> Tetromino Cartesian
maketet = Tetromino . map makecart
  where
    makecart :: (Int,Int) -> Cartesian
    makecart x = Cartesian (fst x) (snd x)

tetromino =
 [
    maketet [(0,0),(1,0),(0,1),(1,1)] -- O
  , maketet [(0,0),(1,0),(2,0),(3,0)] -- I
  , maketet [(0,0),(1,0),(1,1),(2,1)] -- Z
  , maketet [(0,1),(1,1),(1,0),(2,0)] -- S
  , maketet [(0,0),(1,0),(2,0),(2,1)] -- J
  , maketet [(2,0),(1,0),(0,0),(0,1)] -- L
  , maketet [(0,0),(1,0),(2,0),(1,1)] -- T
 ]

-- 平行移動関数
ptrans :: Cartesian -> Tetromino Cartesian -> Tetromino Cartesian
ptrans p = fmap (+p)
-- ptrans p = bind (+ p)

-- 反時計90度の回転関数
simpleRotate :: Tetromino Cartesian -> Tetromino Cartesian
--simpleRotate = Tetromino . map mat . blocks
simpleRotate = fmap mat
  where
    mat :: Cartesian -> Cartesian
    mat c = Cartesian (y c) (negate (x c))
        
-- 中心座標での回転関数
rotate :: Tetromino Cartesian -> Tetromino Cartesian
rotate t = ptrans halfc . simpleRotate . ptrans (negate halfc) $ t
  where
    halfc :: Cartesian
    halfc = head $ tail $ blocks t
    
qsort :: [Cartesian] -> [Cartesian]
qsort (p:xs) = lesser ++ p:greater
  where
    lesser = qsort [y | y <- xs, y < p]
    greater = qsort [y | y <- xs, y >= p]
qsort _ = []

showOne :: Tetromino Cartesian -> String
showOne = showOne' (Cartesian 0 0) . qsort . blocks
showOne' a (b:bs)
  | a == b  = '*' : showOne' right bs
  | not $ (y a) == (y b) = '\n' : showOne' newline (b:bs)
  | otherwise = ' ' : showOne' right (b:bs)
    where
      right = a + (Cartesian 1 0)
      newline = Cartesian 0 ((y a) + 1)
showOne' _ [] = []
