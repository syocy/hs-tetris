module Input where

--import Data.Maybe(isNothing,fromJust)
import System.IO(hWaitForInput,stdin,stdout,hGetBuffering,hSetBuffering,BufferMode(..))

data TInput = TUp | TDown | TLeft | TRight | TEsc | TSpace
             deriving (Show,Eq)

toTInput :: Int -> Maybe TInput
toTInput 65 = Just TUp
toTInput 66 = Just TDown
toTInput 67 = Just TRight
toTInput 68 = Just TLeft
toTInput 27 = Just TEsc
toTInput 91 = Just TEsc
toTInput 32 = Just TSpace
toTInput _ = Nothing

rawInput :: IO (Maybe TInput)
rawInput = do
  char <- getChar
  let c = (fromEnum char)::Int
  let t = toTInput c
  case t of
    Just TEsc -> rawInput
    _ -> return t
--  if c == 27 || c == 91
--    then input
--    else if isNothing $ toTInput c
--      then input
--      then return Nothing
--      else return $ fromJust $ toTInput c
--      else return $ toTInput c

inputWait :: Int -> IO Bool
inputWait = hWaitForInput stdin

input :: Int -> IO (Maybe TInput)
input ms = do
  canInput <- inputWait ms
  if canInput
    then rawInput
    else return Nothing
         
initBufferingMode :: IO ()
initBufferingMode =
  hSetBuffering stdin NoBuffering >>
  hSetBuffering stdout LineBuffering
      
-- main = do
--   initBufferingMode
--   c <- input 2000
--   print c
--   c <- input 2000
--   print c
--   c <- input 2000
--   print c
          

 