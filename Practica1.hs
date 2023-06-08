import System.Win32 (xBUTTON1)
--1
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

serie [] = []
serie [x] = [] : [[x]]
serie xs = serie(borrarUltimo xs) ++ [xs]

paresIguales a b c d = (a==b && c==d) || (a==c && b==d) || (a==d && b==c)

isosceles a b c = (a==b) || (a==c) || (b==c)

ror 0 xs = xs
ror n (x:xs) = ror (n-1) (xs ++ [x])

upto n m = if (n<=m) 
           then [n..m]
           else []

ecoaux 0 x = [x]
ecoaux n x = x : ecoaux (n-1) x

eco' i [] = []
eco' i (x:xs) = ecoaux i x : eco' (i+1) xs

eco xs = concat(eco' 0 xs)


cambios xs = cambios' xs 1
  where cambios' [] _ = []
        cambios' [_] _ = []
        cambios' (x:y:xs) i = if (x /= y) 
                              then i : cambios' (y:xs) (i+1)
                              else cambios' (y:xs) (i+1)

oblongoNumber n = [x * (x+1) | x <- [1..n] ]

divisoresPropios x = [ i | i <-[1..x-1] , x `mod` i == 0  ]

sumalis [] = 0
sumalis (x:xs) = x + sumalis xs

abundantes = [ x | x <- [1..] , x < sumalis(divisoresPropios(x))] 

eco2 xs = concat [replicate i x | (x, i) <- zip xs [1..]]

multiplos x n = [ i | i <- [1..n-1], i `mod` x == 0 ]

euler n = sumalis [ i | i <- [1..n-1], i `mod` 3 == 0  || i `mod` 5 == 0 ] 

expandir xs = [ x | x <- xs, y <- [1..x]]

elevar x = x*x


map' f xs = foldr combine [] xs
            where combine x acc = f x : acc

--2)
--fun1 1 1 True = False
--fun2 True = x == x
--     where x = 1

fun3 ['a'] = ['a']

--fun4 x = if fa x > 1 then [1] else [2]
--       where fa x = 1

--fun7 (a,b,c) = True
fun8 (a,b,c) 2 = c

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

cambios' xs = [i | (x, i) <- zip xs [0..], i > 0, x /= xs !! (i - 1)]

altlenght [] = 0
altlenght (x:xs) = 1+altlenght xs

longitudes [] = []
longitudes (xs:xss) = [altlenght xs] : longitudes xss 

longitudes' xss = map (\ xs -> [altlenght xs]) xss
