import Data.Maybe(isNothing,fromJust)

data TInput = TUp | TDown | TLeft | TRight | TEsc
             deriving (Show,Eq)

toTInput :: Int -> Maybe TInput
toTInput 65 = Just TUp
toTInput 66 = Just TDown
toTInput 67 = Just TRight
toTInput 68 = Just TLeft
toTInput 27 = Just TEsc
toTInput 91 = Just TEsc
toTInput _ = Nothing

input :: IO (Maybe TInput)
input = do
  char <- getChar
  let c = (fromEnum char)::Int
  let t = toTInput c
  case t of
    Just TEsc -> input
    _ -> return t
--  if c == 27 || c == 91
--    then input
--    else if isNothing $ toTInput c
--      then input
--      then return Nothing
--      else return $ fromJust $ toTInput c
--      else return $ toTInput c
      
main = do
  i <- input
  putStrLn ""
  putStrLn $ show i
          

 