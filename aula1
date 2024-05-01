answer :: Int
answer = 42

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x*x

imc :: Float -> Float -> Float
imc p a = p/(a*a)

maxi :: Int -> Int-> Int
maxi n m | n>=m = n
         | otherwise = m

ver :: Int -> Int -> Int -> Bool
ver a b c | a<b && b<c = True
          | otherwise = False

vendas :: Int -> Int
vendas n = mod n 17

checkvendas :: Int -> Int-> Int
checkvendas s n | n < 0 = 0
                | s == vendas n = 1 + checkvendas s (n-1)
                | otherwise = checkvendas s (n-1)

f1vendas :: Int -> Int -> Int 
f1vendas s n | n == 0 = 0
             | n > 0 = checkvendas s n
             | otherwise = 0

primocheck :: Int -> Int -> Bool
primocheck n m | m == 1 = True
               | mod n m == 0 = False
               | otherwise =  primocheck n (m-1)
primo :: Int -> Bool
primo n | n == 1 = True
        | otherwise = primocheck n (n-1) 

primosentresi3 :: Int -> Int -> Int -> Bool
primosentresi3 a b n| mod a n == 0 = False
                    | otherwise = primosentresi2 a b (n+1)

primosentresi2 :: Int -> Int -> Int -> Bool
primosentresi2 a b n| b < n = True 
                    | mod b n == 0 = primosentresi3 a b n
                    | otherwise = primosentresi2 a b (n+1)

primosentresi1 :: Int -> Int -> Bool
primosentresi1 a b | a == b = False
                   | (min a b) == 1 = False
                   |otherwise = primosentresi2 (max a b) (min a b) 2 

