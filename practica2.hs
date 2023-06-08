
data BST a = E | N (BST a) a (BST a)        deriving (Show)




--5)

minimum' (N E x r ) = x
minimum' (N l x r)  = minimum' l

maximum' (N l x E) = x
maximum' (N l x r) = maximum' r

checkBST E = True
checkBST (N E x E) = True
checkBST (N E x r) = checkBST r && (minimum' r) > x
checkBST (N l x E) = checkBST l && (maximum' l) <= x
checkBST (N l x r) = checkBST l && checkBST r && (maximum' l) <= x && (minimum' r) > x



insert' x E = N E x E
insert' x (N l y r) | x > y = (N l y (insert' x r ))
                   | otherwise = (N (insert' x l) y r) 


member' e c E = e == c
member' e c (N l y r) | e > y = member' e c r
                     | otherwise = member' e y l



--peor caso 2 d siendo d la altura

memberr e E = False                      
memberr e (N l y r) | e == y = True
                   | e > y = memberr e r
                   | otherwise = memberr e l


-- arbol completo aquel con todos sus niveles llenos
completo e 0 = E
completo e n = N (completo e (n-1)) e (completo e (n-1))

-- balancea un arbol con un mismo elemento, con una cantidad 
balanceado 0 _ = E
balanceado n x | even (n-1) = let m = div (n-1) 2
                                  t = balanceado m x 
                                 in N t n t 
                |otherwise = let m = div (n-1) 2 
                                 in N ( balanceado (m+1) x ) x (balanceado m x) 


-- crea un arbol a partir de una lista 

fromordlist [] = E
fromordlist xs = let n  = length xs 
                     m = div n 2 
                     x = xs !! m -- toma el elemento de posicion m 
                     zs = take m xs 
                     ys = drop (m+1) xs 
                     (t1,t2)= (fromordlist zs, fromordlist ys) in N t1 x t2
{-
type Cadena = String
type Pos = Int
data Linea  =  (Cadena , Pos)  
-}
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show)

headCL (CUnit x) = x
headCL (Consnoc x xs y) = x

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y


lis = Consnoc 1 (Consnoc 1 EmptyCL 3) 8

isEmptyCL EmptyCL = True
isEmptyCL (CUnit x) = False
isEmptyCL (Consnoc x xs y) = False 


reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) =  CUnit x
reverseCL (Consnoc x xs y) = Consnoc y (reverseCL xs) x

 
