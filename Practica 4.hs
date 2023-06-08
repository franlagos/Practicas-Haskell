data Nat = Cero | Succ Nat deriving (Show)


cinco = Succ (Succ (Succ (Succ (Succ (Cero)))))
tres = Succ (Succ (Succ (Cero)))

int2Nat 0 = Cero
int2Nat n = Succ(int2Nat(n-1))


sumaN Cero y = y
sumaN (Succ x) y = Succ(sumaN x y) 

data Arb = Em | H Int | Nodo Arb Arb

alt Em = 0
alt (H _) = 1
alt (Nodo izq der) = 1 + alt izq + alt der

-- Función para determinar si un árbol Arb es balanceado
esBalanceado Em = True
esBalanceado (H _) = True
esBalanceado (Nodo izq der) = 0 <= (alt izq - alt der) && (alt izq - alt der) <= 1  && esBalanceado izq && esBalanceado der



data Bin a = Hoja | N  (Bin a) a (Bin a) deriving Show

-- ambos member se fijan si un elemento esta en el arbol pero el segundo es mas eficiente
member e Hoja = False 
member e (N l y r ) | e == y = True
                    | e > y = member e r 
                    | otherwise = member e l 

member' e c Hoja = e == c 
member' e c (N  l y r ) | e > y = member' e c r 
                            | otherwise = member' e y l 


cantNodos Hoja d = 0
cantNodos (N l x r ) 0 = 1 
cantNodos (N l x r) d = (cantNodos l (d-1)) + (cantNodos r (d-1))

-- (4b )
altura  Hoja  = 0 
altura  (N l x r )  = 1+ altura l   + altura r   

checkbalancebst  Hoja = True 
checkbalancebst (N l x r ) = (abs ( altura r - altura l ) + 1) <= 1 
-- 4 c 
minimunbst Hoja  = 0
minimunbst (N Hoja x r)  = x
minimunbst (N l x r)  = minimunbst l 

maximunbst Hoja = 0
maximunbst (N l x Hoja)  = x
maximunbst (N l x r)  = maximunbst r

esHoja Hoja Hoja = True 
esHoja _ Hoja = False

pred' arb@(N l x r) val = if ((val <= maximunbst arb) && (val >= minimunbst arb)) then ante arb val 0 else (0,0)

ante (N l x r) val post | val > x = ante r val post  
                        | val < x = ante l val x
                        | otherwise = (if esHoja r Hoja then 0 else sigoante r, if esHoja r Hoja then post else sigoante r )

sigoante (N r x l) = x 

formcrdlist [] = Hoja  
formcrdlist xs = let n  = length xs 
                     m = div n 2 
                     x = xs !! m -- toma el elemento de posicion m 
                     zs = take m xs 
                     ys = drop (m+1) xs 
                     (t1,t2)= (formcrdlist zs, formcrdlist ys) in N t1 x t2

type Rank = Int
data Heap a = E | Node Rank a (Heap a) (Heap a) deriving Show

-- de heap a lista


heap2list E = []
heap2list (Node _ x l r) = x ++ heap2list r ++ heap2list l

orden [] = []
orden (x:xs) = [u| u<- orden xs , u<=x] ++ [x] ++ [i | i<- orden xs, i>x]
    -- (filter (\y  -> y<=x  ) xs) ++ [x] ++ (filter (\y -> y>x  ) xs) 
ordLeftist arb = orden(heap2list (arb)) 


checklh tree@(Node ran z l r) =  rangos l r && menoratodo l r z  && checklh r && checklh l

menoratodo E _ val = True 
menoratodo  _ E val = True 
menoratodo h1@(Node _ x l r) h2@(Node _ y l2 r2) val = val < x  && val < y

rangos  _ E = True
rangos  E _ = False 
rangos (Node ran a l r) (Node ran2 a2 l2 r2) = ran >=  ran2 


{-
data RBT a = Erb | T Color (RBT a) a (RBT a) deriving Show
data Color =  Rojo | B   deriving Show


minimun (T _ Erb x _ )  = x 
minimun ( T _ l x r ) = minimun l


maximun ( T _ _ x Erb ) = x 
maximun ( T _ l x r ) = maximun r

check Erb = True 
check (T _ Erb x Erb)= True 
check (T _ Erb x r)= minimun r > x && check r 
check (T _ l x Erb)= maximun l < x && check l
check (T _ l x r) = maximun l < x && check l && minimun r > x && check r

igual  B  B  = True 
igual  Rojo  Rojo = True
igual  _ _ = False


isRed (T color _ _ _ ) = igual color Rojo

isBlack (T color _ _ _ ) = igual color B


isRedBlackTree Erb = True
isRedBlackTree tree@(T color  _ _ _ ) = isBlack tree && checkRedNodes tree


checkRedNodes Erb = True
checkRedNodes (T B left _ right) = checkRedNodes left && checkRedNodes right
checkRedNodes (T Rojo left _ right) = isBlack left && isBlack right && checkRedNodes left && checkRedNodes right


altB Erb = 0
altB (T B l _ r) = min (1 + altB l) (1 + altB r )
altB (T Rojo l _ r) = min (altB l) (altB r )
             
verificarRBT tree@(T colores l _ r) = altB l == altB r && isRedBlackTree tree && check tree



--rank :: Heap a -> Rank
rank E = 0
rank (Node r _ _ _ ) = r

makeH x a b = if rank a > rank b then Node (rank b + 1) x a b
                                 else Node (rank a + 1) x b a

elimdup ( Node k c a b) = merge' ( Node 1 c E E) (merge' a b)

--merge' :: Heap a -> Heap a -> Heap a
merge' h1 E = cleanup h1
merge' E h2 = cleanup h2
merge' h1@(Node _ x l1 r1) h2@(Node _ y l2 r2) | x < y = makeH x l1 (merge' r1 h2)
                                         | x > y = makeH y l2 (merge' r2 h1)
                                         | otherwise = merge' (merge' l1 r1) h2 

cleanup (Node k x a b) = merge' (Node 1 x E E) (merge' a b)
-}