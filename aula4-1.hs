--(-2, 2) (-1, 2) (0, 2) (1, 2) (2, 2)

--(-2, 1) (-1, 1) (0, 1) (1, 1) (2, 1)

--(-2, 0) (-1, 0) (0, 0) (1, 0) (2, 0)

--(-2,-1) (-1,-1) (0,-1) (1,-1) (2,-1)

--(-2,-2) (-1,-2) (0,-2) (1,-2) (2,-2)

data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

checkLado::[Command] -> Int
checkLado [] = 0
checkLado (TurnLeft:cauda) = (checkLado cauda) + 3
checkLado (TurnRight:cauda) = (checkLado cauda) + 1
checkLado ((Forward n):as) = 0
checkLado ((Backward n):as) = 0

dumpCom :: [Command] -> [Command]
dumpCom [] = []
dumpCom ((TurnLeft):as) = dumpCom as
dumpCom ((TurnRight):as) = dumpCom as
dumpCom ((Forward n):as) = ((Forward n):as)
dumpCom ((Backward n):as) = ((Backward n):as)

dumpCom2 :: [Command] -> [Command]
dumpCom2 [] = []
dumpCom2 ((Forward _):as) = as
dumpCom2 ((Backward _):as) = as
dumpCom2 ((TurnLeft):as) = dumpCom((TurnLeft):as)
dumpCom2 ((TurnRight):as) = dumpCom((TurnRight):as)


op :: (Int,Int) -> [Command] -> Int -> (Int,Int)
op tup [] _ = tup
op tup ((Forward n):as) d 
  | d == 0 = (fst(tup),(snd(tup) + n))
  | d == 1 = ((fst(tup) + n),snd(tup))
  | d == 2 = (fst(tup),(snd(tup) - n))
  | d == 3 = ((fst(tup) - n),snd(tup))

op tup ((Backward n):as) d
  | d == 0 = (fst(tup),(snd(tup) - n))
  | d == 1 = ((fst(tup) - n),snd(tup))
  | d == 2 = (fst(tup),(snd(tup) + n))
  | d == 3 = ((fst(tup) + n),snd(tup))

destination :: (Int,Int) -> [Command] -> (Int,Int)
destination t [] = t 
destination t (c:cs) = destination (op t (dumpCom (c:cs)) (mod (checkLado (c:cs)) 4)) (dumpCom2(c:cs))
