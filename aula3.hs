import Data.Char
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
fibAux 0 = 0
fibAux 1 = 1
fibAux n = fibAux (n-1) + fibAux (n-2)

fib2 :: Int -> Int -> [Int]
fib2 n m | n > (m+3) = [fibAux m] ++ fib2 n (m+3)
         | otherwise = [fibAux m]

fib :: Int -> [Int]
fib n | n < 1 = [0]
      | otherwise = fib2 n 0

qs2 _ [] = []
qs2 (a:as) (pivo:cs) = qs2 as [x|x<-cs,x<=pivo] ++ [a] ++ qs2 as [y|y<-cs,y>pivo]

somAux :: [Int] -> [Int]
somAux [] = []
somAux (a:[]) = [(a `mod` 10) + (a `div` 10)]
somAux (a:as) = [(a `mod` 10) + (a `div` 10)] ++ somAux as

type Ponto = (Float, Float)
type Linha = (Ponto,Ponto)
coord1 :: Ponto -> Float
coord1 x = fst x 
coord2 :: Ponto -> Float
coord2 y = snd y

vert :: Linha -> Bool
vert r | coord1(fst r) == coord1 (snd r) = True
  | otherwise = False

menor :: Int -> Int -> Int -> Int
menor a b c 
  | a < b && a < c = a
  | b < a && b < c = b
  | otherwise = c

maior :: Int -> Int -> Int -> Int
maior a b c 
  | a > b && a > c = a
  | b > a && b > c = b
  | otherwise = c 

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c = (menor a b c, maior a b c)

ordenaTriplaAux :: (Int,Int,Int) -> (Int,Int,Int)
ordenaTriplaAux (a,b,c) 
  | a > b = (b,a,c)
  | otherwise = (a,b,c)

ordenaTripla :: (Int,Int,Int) -> (Int,Int,Int)
ordenaTripla (a,b,c) 
  | a > b && a > c = ordenaTriplaAux (b,c,a) 
  | c > b && c > a = ordenaTriplaAux (a,b,c)
  | otherwise = ordenaTriplaAux (a,c,b)

type Pessoa = String
type Livro = String 
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"), ("Andre","Duna"), ("Fernando","Jonathan Strange & Mr.Norrell"), ("Fernando","A Game of Thrones")]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd p = [l | (pp,l) <- bd, pp == p]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd l = [p|(ll,p) <- bd, ll == l]

emprestado :: BancoDados -> Livro -> Bool
emprestado bd l = (emprestimos bd l) /= []

qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos bd p = length (livros bd p)

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = [(p,l)]
emprestar ((pe,li):as) p l
  | pe == p && li == l = ((pe,li):as)
  | otherwise = ((pe,li):emprestar as p l)

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver ((pessoa,livro):as) p l 
  | pessoa == p && livro == l = as
  | otherwise = (pessoa,livro):devolver as p l
  



getWord :: String -> String
getWord [] = []
getWord (a:as) 
  | a == ' ' = []
  |otherwise = a:getWord as

getWord2 :: String -> String
getWord2 (a:as) = [word| word <- (a:as), word /= ' ']

dropWord :: String -> String
dropWord [] = []
dropWord (a:as) 
  | a == ' ' = (a:as)
  |otherwise = dropWord as

dropSpace :: String -> String
dropSpace [] = []
dropSpace (a:as) 
  | a /= ' ' = (a:as)
  |otherwise = dropSpace as

splitWords :: String -> [String]
splitWords [] = []
splitWords (a:as) = (getWord (a:as)):splitWords(dropSpace(dropWord (a:as)))

type Palavra = String
type Line = [String]

getLinha :: Int -> [Palavra] -> Palavra
getLinha _ [] = []
getLinha n (a:as) 
  |n == 1 = a
  |otherwise = getLinha (n-1) as

dropLine :: Int -> [Palavra] -> [Palavra]
dropLine _ (a:[[]]) = []
dropLine n (a:as)
  |  n <= 0 = (a:as)
  |  n == 1 = as
  |  otherwise = a:dropLine (n-1) as

qs [] = []
qs (pivo:as) = qs([maiores|maiores<-as,maiores>pivo]) ++ [pivo] ++ qs([menores|menores<-as,menores<=pivo])
