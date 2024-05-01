import Prelude hiding (Maybe (..))

data Maybe a = Just a |
               Nothing
               deriving(Show)


op :: String -> String -> String -> Maybe Int
op s n1 n2
  | s == "sum" = Just ((read(n1)::Int) + (read(n2)::Int))
  | s == "sub" = Just ((read(n1)::Int) - (read(n2)::Int))
  | s == "mul" = Just ((read(n1)::Int) * (read(n2)::Int))
  | s == "div" && (read(n2)::Int) == 0 = Nothing
  | s == "div" = Just ((read(n1)::Int) `div` (read(n2)::Int))
  | otherwise = Nothing

num :: String -> String
num [] = []
num (a:as) 
  |a == 's' || a == 'm' || a == 'd' = []
  |otherwise = [a] ++ num as 

udNum :: String -> String
udNum [] = []
udNum (a:as)
  |a == 's' || a == 'm' || a == 'd' = (a:as)
  |otherwise = udNum as

udOp :: String -> String
udOp [] = []
udOp (a:as)
  |a == '0' || a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9' = [a] ++ udOp as
  |otherwise = udOp as

strOp :: String -> String
strOp [] = []
strOp (a:as) 
  |a == '0' || a == '1' || a == '2' || a == '3' || a == '4' || a == '5' || a == '6' || a == '7' || a == '8' || a == '9' = []
  |otherwise = [a] ++ strOp as

safeCalc :: String -> IO ()
safeCalc [] =  putStrLn $ show (Nothing::(Maybe Int))
safeCalc s =  putStrLn $ show (op(strOp(udNum(s))) (num(s)) (udOp(udNum(s))))

