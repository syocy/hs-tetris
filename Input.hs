module Input where

--import Data.Maybe(isNothing,fromJust)
import System.IO(hReady,hWaitForInput,stdin,stdout,hGetBuffering,hSetBuffering,BufferMode(..))
import Data.Maybe(fromJust)
import System.Posix.Unistd(usleep)

data TInput = TInput {
  key :: Key,
  frame :: Int
  } deriving(Show,Eq)

data Key = TUp | TDown | TLeft | TRight | TEsc | TSpace
             deriving (Show,Eq)

toKey :: Int -> Maybe Key
toKey 65 = Just TUp
toKey 66 = Just TDown
toKey 67 = Just TRight
toKey 68 = Just TLeft
toKey 27 = Just TEsc
toKey 91 = Just TEsc
toKey 32 = Just TSpace
toKey _ = Nothing

rawInput :: IO (Maybe Key)
rawInput = do
  char <- getChar
  let c = (fromEnum char)::Int
  let t = toKey c
  case t of
    Just TEsc -> rawInput
    _ -> return t

inputWait :: Int -> IO Bool
inputWait = hWaitForInput stdin

inputWithDelay :: Int -> IO (Maybe Key)
inputWithDelay ms = do
  canInput <- inputWait ms
  if canInput
    then rawInput
    else return Nothing
         
initBufferingMode :: IO ()
initBufferingMode =
  hSetBuffering stdin NoBuffering >>
  hSetBuffering stdout LineBuffering
  
input :: TInput -> IO (Maybe TInput)
input old = do
  readable <- hReady stdin
  if not readable
    then return Nothing
    else do
      nowKey <- rawInput
      if nowKey == Nothing
        then return Nothing
        else return . Just . TInput (fromJust nowKey) $
          if key old == fromJust nowKey
            then (frame old) + 1
            else 0
            
inputTest = do
  initBufferingMode
  usleep 1000000
  x <- input $ TInput TUp 0
  print x
  
--input :: GameVariables -> IO (Maybe TInput)
--input = undefined
      
main = do
  initBufferingMode
  loop
  where loop = do
          c <- inputWithDelay 2000
          print c
          loop
--   c <- inputWithDelay 2000
--   print c
