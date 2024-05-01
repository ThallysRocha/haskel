agrupar :: Eq t => [t] -> [(t,Int)]
agrupar [] = []
agrupar (a:as) = (a,length([iguais|iguais<-(a:as),iguais == a])):agrupar [diferentes|diferentes<-(a:as),diferentes /= a]

agrupar2 :: [[t]] -> [t]
agrupar2 xs = foldr (++) [] xs

agrupar1 :: Eq t => [[t]] -> [(t,Int)]
agrupar1 list = agrupar (agrupar2 list)

double x = 2*x

iter :: Int -> (t -> t) -> (t -> t)
iter 0 f = id
iter n f = (iter (n-1) f).f
