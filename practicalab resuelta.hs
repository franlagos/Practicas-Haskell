--2)

five x = 5

apply f x = f x

id x = x

sign x | x < 0 = -1
       | x > 0 = 1
       | otherwise = 0

vabs x | sign x == -1 = -x 
       | sign x == 1 = x 
       | sign x == 0 = 0 

max3 a b c   | a >= b && a >= c = a  
             | b >= a && b >= c = b 
             | c >= b && c >= b = c  



--5)

divisor x = [ i | i <-[1..x] , x `mod` i == 0 ]

matches y xs = [ i | i <- (xs), i == y ]

cuadrupla n = [(a, b, c, d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n]
                , a^2 + b^2 == c^2 + d^2]



--7)

suma [] = 0
suma (x:xs) = x + suma xs

sum xs = foldr (+) 0 xs

ordinales = [(x,i) | (x,i) <- zip ['a'..'z'] [1..] ]

codes xs = [ i | j <- xs, (x,i) <- ordinales, x==j ]


modul a b | a < b = a
          | otherwise = modul (a-b) b

restos xs n = [ modul x n | x <- xs ]


elevar x = x*x

cuadrados [] = []
cuadrados (x:xs) = elevar x: cuadrados xs

cuadrad xs = map elevar xs

long [] = 0
long (x:xs) = 1 + long xs

longitudes [] = []
longitudes (x:xs) = long x : longitudes xs



maytri (x,y) = x < 3*y

orden ((x,y):xs) = [ (x,y) | (x,y) <- xs , maytri (x,y)   ]

pares xs = [ x | x <- xs , even x]

letras xs =  [ x | x <- xs , (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')  ] 

lenght [] = 0
lenght (x:xs) = 1 + lenght xs 

lenght' xs = foldr (\ x -> (+) 1) 0 xs

masDe n xss = [ xs | xs <- xss , lenght xs > n] 

