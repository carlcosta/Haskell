{-# LANGUAGE BlockArguments #-}
module Questoes50 where

import Data.List ()

-- exercicio 1

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y 
    | x > y = []
    | otherwise = x:enumFromTo' (x+1) y

--exercicio 2

enumFromThenTo' :: Int -> Int-> Int -> [Int]
enumFromThenTo' x value y
    | x > y && value - x > 0 || x < y && value - x < 0 = []
    | otherwise = x : enumFromThenTo' value (2*value - x) y

-- exercicio 3

concaneta :: [a] -> [a] -> [a]
concaneta[] l = l
concaneta (h:t) l = h:concaneta t l

--exercicio 4

elemLista :: [a] -> Int -> a
elemLista (h:t) x
        | x == 0 = h
        | otherwise = elemLista t (x-1)
          
--exercico 5

reverse' :: [a] -> [a] 
reverse' [] = []
reverse' (x:xs) = reverse' (xs) ++ [x]

--exercicio 6

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' x (h:t) = h : take' (x - 1) t

--exercicio 7

drop' ::  Int -> [a] -> [a]
drop' 0 l = l
drop' _ [] = []
drop' x (_:t) = drop' (x - 1) t

--exercicio 8

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (h:t) = (x,h) :zip' xs t

--exercicio 9

elem' ::  Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t) = x == h || elem' x t 
 
--exercicio 10

replicate' ::  Int -> a -> [a]
replicate' 0 _ = []
replicate' x xs = xs:replicate' (x-1) xs

--exercicio 11

intersperse' ::  a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [h] = [h]
intersperse' n (x:xs) = x:n:intersperse' n xs

--exercicio 12 
{-
group' ::  Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = (h:aux t) : aux2 t
    where aux (x:xs) = if x == h then x:aux xs else []
          aux [] = []
          aux2 [] = []
          aux2 (y:ys) = if y == h then aux2 ys else group' (y:ys) l 

-------------------------------------------------------------------------
group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' (h:t) = (h:takeWhile (== h) t) : group' (dropWhile (== h) t
-}

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' l@(h:_) = x : group' y
    where
      (x,y) = span (==h) l
      
--exercicio 13

concat' ::  [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--exercicio 14

inits' ::  [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++[l]

--inits''

--exercicio 15

tails' ::  [a] -> [[a]]
tails' [] = [[]]
tails' l = l:tails' (tail l )

--exercicio 16

isPrefixOf' ::  Eq a => [a]-> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (x:xs) = h == x && isPrefixOf' t xs

--exercicio 17

isSuffixOf' ::  Eq a => [a]-> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' l1 l2 = (last l1 == last l2) && isSuffixOf' (init l1) (init l2) 

--exercicio 18

isSubsequenceOf' ::  Eq a =>[a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (x:xs) | h == x = isSubsequenceOf' t xs 
                              | otherwise = isSubsequenceOf' (h:t) xs   

--exercicio 19 

aux :: (Eq a,Num a )=> a -> [a] -> Int-> [Int]
aux _ [] _ = []
aux n (h:t) p | (n == h ) = p : aux n t (p+1)
              | otherwise = aux n t  (p+1 )

elemIndices' ::  (Eq a,Num a )=> a ->[a] -> [Int]
elemIndices' _ [] = []
elemIndices' n (h:t) = aux n (h:t) 0

--exercicio 20

nub' ::  Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if h `elem` t then nub' t else h:nub' t

--exercicio 21

delete' ::  Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (h:t) = if n == h then t else h : delete' n t

--exercicio 22

barras ::  Eq a => [a] -> [a]-> [a]
barras [] _ = []
barras l [] = l 
barras l (h:t) = (barras (delete' h l) t )

--exercicio 23

union' ::  Eq a => [a] -> [a]-> [a]
union' l [] = l
union' [] l = l
union' l (h:t) = if h `elem` t then union' l t else union' ((++) l [h]) t

--exercicio 24

intersect' ::  Eq a => [a] ->[a] -> [a]
intersect' l [] = l
intersect' [] _ = []
intersect' l (h:t) = if  h `elem` l then h : intersect' l t else intersect' l t

--exercicio 25

insert' ::  Ord a => a -> [a]-> [a]
insert' n [] = [n]
insert' n (h:t) = if n > h then (h : insert' n t) else n:h:t

--exercicio 26

unwords' ::  [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords' t

--exercicio 27

unlines' ::  [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

--exercicio 28

pMaior'::  Ord a => [a] -> Int
pMaior' l = pMaiorAux 0 (maximum l) l

pMaiorAux :: Ord a => Int -> a -> [a] -> Int
pMaiorAux x _ [] = x
pMaiorAux x y (h:t) = if y == h then pMaiorAux x y [] else pMaiorAux (succ x) y t

--exercicio 29 

temRepetidos' ::  Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (h:t) = h `elem` t  ||  temRepetidos' t

--exercicio 30

algarismos' ::  [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t) = if h `elem` ['0'..'9'] then h:algarismos' t else algarismos' t

--exercicio 31

posImpares' ::  [a] -> [a]
posImpares' [] = []
posImpares' [_] = []
posImpares' (h:s:t) = s:posImpares' t

--exercicio 32

posPares' :: [a] -> [a]
posPares' [] = []
posPares' [x] = [x]
posPares' (h:s:t) = h:posPares' t

--exercicio 33

isSorted' ::  Ord a => [a] -> Bool
isSorted' [] = True
isSorted' [_] = True
isSorted' (h:s:t) = s >= h && isSorted' (s:t)

--exercicio 33.1

isSorted2 :: Ord a => [a] -> Bool
isSorted2 [] = True
isSorted2 (h:s:t) = if s>=h 
                    then if null t 
                        then True 
                        else isSorted2 (s:t)
                    else False

--exercicio 34

iSort' ::  Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert' h (iSort' t)

--exercicio 35

menor' ::  String -> String -> Bool
menor' _ [] = False 
menor' [] _ = True
menor' x y = if length x > length y then False else True 

--exercicio 36 

elemMSet' ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' n ((a,x):t) =  n == a || elemMSet' n t


elemMSet2 ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet2 _ [] = False
elemMSet2 n ((a,x):t) = if n == a then True else elemMSet' n t

--exercicio 37

lengthMSet' ::  [(a,Int)] -> Int
lengthMSet' [] = 0
lengthMSet' ((x,n):t) = n + lengthMSet' t

--exercicio 38

converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,1):t) = x:converteMSet' t
converteMSet' ((x,n):t) = x:converteMSet' ((x,n-1):t)

--exercicio 39

insereMSet' ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' x [] = [(x,1)]
insereMSet' x ((a,n):t) = if x == a then ((a,n+1)) : t else (a,n) : insereMSet' x t

--exercicio 40

removeMSet' ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' _ [] = []
removeMSet' x ((a,n):t) = if x == a then if n == 1 then t 
                          else (a,(n-1)) : t 
                          else (a,n) : removeMSet' x t

--exercicio 41

constroiMSet' ::  Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (h:t) = constroiMSetAux h (constroiMSet' t)

constroiMSetAux ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
constroiMSetAux x [] = [(x,1)]
constroiMSetAux x ((a,n):t) = if x == a then ((a,n+1)) : t else (a,n) : insereMSet' x t

--exercicio 42

partitionEithers' ::  [Either a b] -> ([a],[b])
partitionEithers' l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left h):t) = h:partitionLefts t
          partitionLefts ((Right _):t) = partitionLefts t
          partitionRights [] = []
          partitionRights ((Left _):t) = partitionRights t
          partitionRights ((Right h):t) = h:partitionRights t

--exercicio 43

catMaybe' ::  [Maybe a] -> [a]
catMaybe' [] = []
catMaybe' (Just x:t) = x : catMaybe' t
catMaybe' (Nothing : t) = catMaybe' t

--exercicio 44

data Movimento = Norte | Sul | Este | Oeste
                deriving Show

posicao ::  (Int,Int) -> [Movimento] -> (Int,Int)
posicao x [] = x
posicao (x,y) (Norte:t) = posicao (x,y+1) t
posicao (x,y) (Sul:t) = posicao (x,y-1) t
posicao (x,y) (Este:t) = posicao (x+1,y) t
posicao (x,y) (Oeste:t) = posicao (x-1,y) t

--exercicio 45

caminho ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1 > x2 = Oeste : caminho (x1-1,y1) (x2,y2)
                        | x1 < x2 = Este : caminho (x1+1, y1) (x2, y2)
                        | y1 > y2 = Sul : caminho (x1, y1-1) (x2, y2)
                        | y1 < y2 = Norte : caminho (x1, y1+1) (x2, y2)
                        | otherwise = []

--exercicio 46

vertical ::  [Movimento] -> Bool
vertical [] = True
vertical (Norte:t) = True && vertical t
vertical (Sul:t) = True && vertical t
vertical (Este:t) = False && vertical t
vertical (Oeste:t) = False && vertical t

--exercicio 47

data Posicao = Pos Int Int
              deriving Show

maisCentral ::  [Posicao] -> Posicao
maisCentral [Pos x y] = Pos x y
maisCentral ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) 
                                       then maisCentral (Pos x y : ps)
                                       else maisCentral (Pos a b : ps)

--exercicio 48

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1):t) | (y == y1) && (x == (x1 +1)) = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | (y == y1) && (x == (x1 -1)) = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | (x == x1) && (y == (y1 +1)) = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | (x == x1) && (y == (y1 -1)) = (Pos x1 y1) : vizinhos (Pos x y)  t
                                   | otherwise = vizinhos (Pos x y) t

--exercicio 49

mesmaOrdenada ::  [Posicao] -> Bool
mesmaOrdenada [] = True
mesmaOrdenada [x] = True
mesmaOrdenada ((Pos x1 y1):(Pos x2 y2): t) = if x1 == x2 
                                             then mesmaOrdenada ((Pos x2 y2):t) 
                                             else False

--exercicio 50

data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK l = interseccaoOK_aux l False
    where interseccaoOK_aux :: [Semaforo] -> Bool -> Bool
          interseccaoOK_aux [] _ = True
          interseccaoOK_aux (Vermelho:t) v = interseccaoOK_aux t v 
          interseccaoOK_aux (_ :t) False = interseccaoOK_aux t True 
          interseccaoOK_aux (_ :t) True = False 


