module Tetris where

import Game
import Input
import DataType
import Util

import Data.List(intersect,sort,permutations)
import Data.Maybe(fromJust,isJust,isNothing)
import Bits(xor,shiftR,shiftL)
import CPUTime(getCPUTime)
import System.Posix.Unistd(usleep)
import System.Cmd

fieldHeight = 15
fieldWidth  = 10

--data Tetris = Tetris (GameVariables,[GameObject])
data Tetris = Tetris {
  gameVariables :: GameVariables,
  gameObjects :: [GameObject]
}
data GameVariables = GameVariables {
    level :: Int
  , randoms :: [Int]
  , count :: Int
} deriving(Show,Eq)
data GameObject =
  TetrominoObj {
      position :: Cartesian
    , content :: Tetromino Cartesian
--    , degree :: Int
  } |
  OldObj {
      position :: Cartesian
    , content :: Tetromino Cartesian
  } |
  PileObj {
    content :: Tetromino Cartesian
  } deriving(Show,Eq)
            
instance Ord GameObject where
  (<=) (TetrominoObj _ _) (OldObj _ _) = True
  (<=) (TetrominoObj _ _) (PileObj _) = True
  (<=) (OldObj _ _) (PileObj _) = True
  (<=) _ _ = False
  
getTetrominoObj :: [GameObject] -> Maybe GameObject
getTetrominoObj = foldl f Nothing where
  f Nothing (TetrominoObj p c) = Just $ TetrominoObj p c
  f Nothing _ = Nothing
  f (Just x) _ = Just x
getOldObj = foldl f Nothing where
  f Nothing (OldObj p c) = Just $ OldObj p c
  f Nothing _ = Nothing
  f (Just x) _ = Just x
getPileObj = foldl f Nothing where
  f Nothing (PileObj c) = Just $ PileObj c
  f Nothing _ = Nothing
  f (Just x) _ = Just x
  
-- concat . filter topTetrominop . permutations where
-- topTetrominop :: [GameObject] -> Bool
-- topTetrominop ((TetrominoObj _ _):_) = True
-- topTetrominop _ = False
            
turnPile :: [GameObject] -> [GameObject]
turnPile go = PileObj c : [] where
  (mo, mp) = (getOldObj go, getPileObj go)
  c = case (mo, mp) of
    (Nothing, Nothing) -> error "turnPile"
    (Nothing, _) -> content $ fromJust mp
    (_, Nothing) -> transform $ fromJust mo
    (_, _) -> (content $ fromJust mp) <++> (transform $ fromJust mo)
   -- getOldObj go && isNothing $ getPileObj go = 
   --  | isNothing $ getOldObj go = content . fromJust . getPileObj $ go
   --  | isNothing $ getPileObj go = transform . fromJust . getOldObj $ go
   --  | otherwise = (content . fromJust . getPileObj) go
   --                <++> (transform . fromJust . getOldObj) go
  
rawDeletePile :: Int -> [GameObject] -> [GameObject]
rawDeletePile y = map close where
  close :: GameObject -> GameObject
  close (PileObj p) =
    PileObj . Tetromino . map close' . filter noHitp $ blocks p
  close x = x
  noHitp :: Cartesian -> Bool
  noHitp (Cartesian _ y') = not $ y == y'
  close' (Cartesian x' y') | y'<y = Cartesian x' (y'+1)
                           | otherwise = Cartesian x' y'
                                         
deletePile :: [GameObject] -> [GameObject]
deletePile go = let mp = getPileObj go in
  case mp of
    Nothing -> go
    Just po -> fullp 0 . sort . blocks . content $ po where
      fullp :: Int -> [Cartesian] -> [GameObject]
      fullp n t | n==fieldHeight = go
                | otherwise = let s = span ((==) n . y) t in
                if length (fst s) == fieldWidth
                  then rawDeletePile n go
                  else fullp (n+1) (snd s)
    
transform :: GameObject -> Tetromino Cartesian
transform (TetrominoObj {position=p, content=t}) = ptrans p t
transform (OldObj {position=p, content=t}) = ptrans p t
transform x = content x

-- untransform :: Tetromino Cartesian -> GameObject
-- untransform t = TetrominoObj p c where
--   p = offset
--   c = ptrans offset t
--   offset = Cartesian xoffset yoffset
--   xoffset = maximum . toXs $ t
--   yoffset = maximum . toYs $ t
  
fieldOverp :: Tetromino Cartesian -> Bool
fieldOverp = any fieldOverp' . toPairs
  where fieldOverp' :: (Int,Int) -> Bool
        fieldOverp' (x,y)
          | x < 0 || x >= fieldWidth || y < (-1) || y >= fieldHeight = True
          | otherwise = False
    
collidep :: Tetromino Cartesian -> [GameObject] -> Bool
collidep t objs = any collidep' objs
  where collidep' :: GameObject -> Bool
        collidep' (PileObj p) = not . null $ blocks t `intersect` blocks p
        collidep' _ = False
        
rollback :: [GameObject] -> [GameObject]
rollback go =
  let o = fromJust . getOldObj $ go in
  let t = TetrominoObj (position o) (content o) in
  t : o : case getPileObj go of
    Nothing -> []
    Just x -> [x]
    
collide :: [GameObject] -> [GameObject]
collide go = let mt = getTetrominoObj go in
  case mt of
    Nothing -> go
    Just t | collidep (transform t) go || fieldOverp (transform t)
             -> rollback go
           | otherwise -> go
--             -> if canFall go then rollback go else turnPile go
--  collidep (transform t) go 
--             -> (x $ position t) == (x $ position $ fromJust $ getOldObj go) 
    
fall :: GameVariables -> [GameObject] -> [GameObject]
fall gv go = --if isNothing $ getTetrominoObj go then error "fall" else
  let oldt = fromJust $ getTetrominoObj go in
  let newt = TetrominoObj ((Cartesian 0 1) + (position oldt)) (content oldt) in
  let old = OldObj (position oldt) (content oldt) in
  let pile = if isNothing $ getPileObj go
               then []
               else [fromJust $ getPileObj go] in
  let newgo = newt : old : pile in
  if (fieldOverp (transform newt) || collidep (transform newt) go) 
    then turnPile newgo
    else if mod (count gv) 20 == 0 then newgo else go
--  newt : (OldObj (position oldt) (content oldt)) : (if isNothing $ getPileObj go then [] else [fromJust $ getPileObj go])
                          
-- canFall :: [GameObject] -> Bool
-- canFall go = let mt = getTetrominoObj go in
--   case mt of
--     Nothing -> error "canFall"
--     Just t -> let t' = transform . fromJust . getTetrominoObj . fall $ go in 
--       not $ collidep t' go || fieldOverp t'
        
getExceptTetrominoObj :: [GameObject] -> [GameObject]
getExceptTetrominoObj go = 
  let mo = getOldObj go in
  let mp = getPileObj go in
  case (mo, mp) of
    (Nothing, Nothing) -> []
    (Nothing, _) -> [fromJust mp]
    (_, Nothing) -> [fromJust mo]
    (_, _) -> [fromJust mo, fromJust mp]
    
moveOffset :: Key -> Cartesian
moveOffset TLeft = Cartesian (-1) 0
moveOffset TRight = Cartesian 1 0
moveOffset TDown = Cartesian 0 1

move :: TInput -> [GameObject] -> [GameObject]
move tinput go = let mt = getTetrominoObj go in
  case mt of
    Nothing -> error "move"
    Just to -> 
      if isNothing $ getPileObj go
        then [to', oo']
        else [to', oo', po] where
          oo = if isNothing $ getOldObj go then error "move" else fromJust $ getOldObj go
          rltrans =
            TetrominoObj ((position to) + (moveOffset $key tinput)) (content to)
          (to', oo') = case key tinput of
            TLeft -> (rltrans, OldObj (position to) (content to))
            TRight -> (rltrans, OldObj (position to) (content to))
            TSpace -> (TetrominoObj (position to) (rotate $ content to), 
                       OldObj (position to) (content to))
            TDown  -> (rltrans, OldObj (position to) (content to))
            _ -> (to, oo)
--          oo' = OldObj (position to) (content to)
          po = fromJust $ getPileObj go
                          
-- move :: TInput -> [GameObject] -> [GameObject]
-- move tinput go = let mt = getTetrominoObj go in
--   case mt of 
--     Nothing -> error "move"
--     Just t -> 
--       if isNothing $ getPileObj go
--         then [t', o']
--         else [t', o', p] where
--           o = fromJust $ getOldObj go
--           post = position t
--           cont = content t
--           trat = transform t
--           minx = minimum $ toXs trat
--           maxx = maximum $ toXs trat
--           maxy = maximum $ toYs trat
--           ptrans' cart = TetrominoObj (cart + post) cont
--           (t', o') = case key tinput of
--             TLeft  | minx==0 -> (t, o)
--                    | otherwise -> (ptrans' (Cartesian (-1) 0), t)
--             TRight | maxx==fieldWidth-1 -> (t, o)
--                    | otherwise -> (ptrans' (Cartesian 1 0), t)
--             TDown  | maxy==fieldHeight-1 -> (t, o)
--                    | otherwise -> (ptrans' (Cartesian 0 1), t)
--             TSpace -> let rotated = transform $ TetrominoObj post (rotate cont) in
--               if collidep rotated go || fieldOverp rotated
--                 then (t, o)
--                 else (TetrominoObj post (rotate cont), t)
--             _ -> (t, o)
--           p = fromJust $ getPileObj go
          
--          o' = OldObj post cont
--TetrominoObj post (rotate cont)      
    -- Just t -> t' : getExceptTetrominoObj go where
    --   t' = case key tinput of
    --     TLeft  -> TetrominoObj ((Cartesian (-1) 0) + (position t)) (content t)
    --     TRight -> TetrominoObj ((Cartesian 1 0)    + (position t)) (content t)
    --     TDown  -> TetrominoObj ((Cartesian 0 1)    + (position t)) (content t)
    --     TSpace -> TetrominoObj (position t) (rotate $ content t)
    --     _ -> t
        
xorshift :: Int -> Int -> Int -> Int -> [Int]
xorshift x y z w =
  w' : xorshift y z w w' where
    t = x `xor` (x `shiftL` 11)
    w' = (w `xor` (w `shiftR` 19)) `xor` (t `xor` (t `shiftR` 8))
    
nextRandom :: GameVariables -> GameVariables
nextRandom gv = GameVariables l v f where 
  l = level gv
  v = drop 10 $ randoms gv
  f = count gv
  
counting :: GameVariables -> GameVariables
counting gv = GameVariables l v f where
  l = level gv
  v = randoms gv
  f = 1 + count gv
  
getRandom :: Int -> Int -> GameVariables -> Int
getRandom m n = flip mod m . abs . head . drop n . randoms
        
create :: GameVariables -> [GameObject] -> [GameObject]
create gv go
  | isJust $ getTetrominoObj go = go
  | otherwise = newt : newo : p where
      r m n = getRandom m n gv
      newt = TetrominoObj (Cartesian 3 (-1)) (head $ drop (r 6 0) tetromino)
      newo = OldObj (Cartesian 3 (-2)) (head $ drop (r 6 0) tetromino)
      p | isNothing $ getPileObj go = []
        | otherwise = [fromJust $ getPileObj go]
        
allCoors :: [GameObject] -> [Cartesian]
allCoors ((OldObj _ _):xs) = allCoors xs
allCoors (x:xs) = (blocks $ transform x) ++ allCoors xs
allCoors [] = []

stringForRender :: [GameObject] -> String
stringForRender = 
  flip (++) underline . stringForRender' 0 . sort . allCoors
    where
      underline :: String
      underline = replicate (fieldWidth+2) '-' ++ "\n"
      stringForRender' :: Int -> [Cartesian] -> String
      stringForRender' n []
        | n >= fieldHeight = []
        | otherwise = concat $ replicate (fieldHeight-n) $ '|' : replicate (fieldWidth) ' ' ++ "|\n"
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
updateTetris tinput (Tetris variables objects)  
  = Tetris newVariables newObjects where
  newVariables = (counting . nextRandom) variables
  newObjects =
    (deletePile . fall variables . collide . move tinput . create variables)
    objects

renderTetris :: Tetris -> IO ()
renderTetris (Tetris gv go) = putStr $ stringForRender go -- ++ show go
    
isTetrisOver :: Tetris -> Bool
isTetrisOver (Tetris gv go) = any over go
  where
    over :: GameObject -> Bool
    over (PileObj p) = 0 > (minimum . toYs $ p)
    over _ = False
    
initialTetris :: IO Tetris
initialTetris = do
  seed <- getCPUTime
  let initialVariables =
        GameVariables 1 (xorshift 1 2 3 (fromInteger seed)) 0
  let initialObjects = []
  return $ Tetris initialVariables initialObjects
  
initialTInput :: TInput
initialTInput = TInput TNoInput 0
    
main = do
  initBufferingMode
  initialGame <- initialTetris
  loop initialGame initialTInput where
    loop game oldInput = 
      if isGameover game
        then do
          putStrLn "GameOver"
          x <- getChar
          main
        else do
          rawSystem "clear" []
          render game
          newInput <- input oldInput
          usleep 30000 --000
          let newGame = update newInput game
          loop newGame newInput
            
