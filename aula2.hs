import Data.Char
puts :: Int -> String -> String
puts n str | n > 0 = puts (n-1) ((++) str " ")
           | otherwise = str

addSpace :: Int -> String
addSpace n | n == 0 = ""
           |otherwise = puts (n-1) (" ")

paraDireita :: Int -> String -> String
paraDireita n s | n == 0 = s
                | otherwise = (++) (addSpace n) s


vendas :: Int -> Int
vendas n = mod n 17

vendasTotal :: Int -> Int
vendasTotal n | n > 0 = (vendas n) + (vendasTotal (n-1))
              | otherwise = 0

mediaVendas :: Int -> Float
mediaVendas n = t / q
  where t = fromIntegral (vendasTotal n) :: Float
        q = fromIntegral n :: Float

imprimeSemanas :: Int -> String
imprimeSemanas n | n >= 0 = imprimeSemanas (n-1) ++ "\n  " ++ show (n) ++ "     " ++ show(vendas n)
                 | otherwise = ""

imprimeTotal :: Int -> String
imprimeTotal n = "\nTotal"  ++ "   " ++ show(vendasTotal n)

media :: Int -> String
media n = "\nMedia"  ++ "   " ++ show(mediaVendas n)

imprimeTabela :: Int -> String
imprimeTabela n = ("Semana Venda" ++ (imprimeSemanas n) ++ (imprimeTotal n) ++ (media n) ++ "\n")


double :: [Int] -> [Int]
double [] = []
double (a:as) = [2*a] ++ double as

member :: [Int] -> Int -> Bool
member [] _ = False
member (a:as) x 
  | x == a = True
  | otherwise = member as x  

digits :: String -> String
digits [] = []
digits (a:as)
  |(isDigit a) = [a] ++ digits as
  |otherwise = digits as

sumPairs :: [Int] -> [Int] -> [Int]
sumPairs [] [] = []
sumPairs [] (b:bs) = [b] 
sumPairs (a:as) [] = [a] 
sumPairs (a:as) (b:bs) = [a+b] ++ sumPairs as bs


fibAux :: Int -> Int
fibAux 1 = 0
fibAux 2 = 1
fibAux n = fibAux (n-1) + fibAux (n-2)

fib2 :: Int -> Int -> [Int]
fib2 n m 
  | n == m = [fibAux n]
  | otherwise = [fibAux m] ++ [fibAux (m+1)]

