module One where

import Data.List

null' [] = True
null' x = False

take' n (a:b)
  | b == [] = []
  | n <= 0 = []
  | otherwise = a : take' (n-1) b

drop' n (a:b)
  | b == [] = []
  | n <= 0 = (a:b)
  | otherwise = drop' (n-1) b

map' n [] = []
map' n (a:b) = (n a) : (map' n b)

filter' n [] = []
filter' n (a:b)
  | (n a) == True = a : filter' n b
  | (n a) == False = filter' n b

fst' (x,y) = x

snd' (x,y) = y

delete' n [] = []
delete' n (a:b)
  | a == n = [] ++ b
  | otherwise = [a] ++ delete' n b

deleteAll n [] = []
deleteAll n (a:b)
  | a == n = deleteAll n b
  | otherwise = [a] ++ deleteAll n b

nth (a:b) 0 = a
nth (a:b) x = nth b (x-1)

elem' z [] = False
elem' z (a:b)
  | z == a = True
  | z /= a = elem' z b

notElem' z [] = True
notElem' z (a:b)
  | z == a = False
  | z /= a = notElem' z b

head' (a:b) = a

length' [] = 0
length' (a:b) = 1 + (length' b)

reverse' [] = []
reverse' (a:b)
  | otherwise = reverse' b ++ [a]

last' (a:b)
  | b == [] = a
  | otherwise = last' b

tail' (a:b) = b

init' [z] = []
init' (a:b)
  | otherwise = a : init' b

max' x y
  | (x >= y) = x
  | (y >= x) = y

min' x y
  | (x <= y) = x
  | (y <= x) = y

concat' [] = []
concat' [z] = z
concat' (a:b) = a ++ concat' b

intersperse' x [] = []
intersperse' x (a:b)
  | b == [] = a : intersperse' x b
  | otherwise = a : x : intersperse' x b

intercalate' (s:d) [] = []
intercalate' (s:d) (a :b)
  | b == [] = a ++ intercalate' (s:d) b
  | otherwise = a ++ (s:d) ++ intercalate' (s:d) b

and' [True] = True
and' (a:b)
  | a == False = False
  | a == True = and' b

or' [False] = False
or' (a:b)
  | a == True = True
  | a == False = or' b

sum' [] = 0
sum' (a:b) = a + (sum' b)

product' [] = 1
product' (a:b) = a * (product' b)

takeWhile' n [] = []
takeWhile' n (a:b)
  | n a == False = []
  | otherwise = a : takeWhile' n b

dropWhile' n [] = []
dropWhile' n (a:b)
  | n a == True = b
  | otherwise = (a:b)

concatMap' s [] = []
concatMap' s (a:b) = (s a) ++ concatMap' s b

all' n [] = True
all' n (a:b)
  | n a == False = False
  | n a == True = all' n b

any' b [] = False
any' n (a:b)
  | n a == False = any' n b
  | n a == True = True

insert' s [] = [s]
insert' s (a:b)
  | (min s a) == s = s : a : b
  | (min s a) == a = a : insert' s b

nub' [] = []
nub' (a:b) = a : nub' (deleteAll a b)

sort' [] = []
sort' (a:b) = (minimum' (a:b)) : sort' (delete' (minimum' (a:b)) (a:b) )

minimum' [a] = a
minimum' (a:b) = minimum' ((min' a (head' b)):tail' b)

maximum' [a] = a
maximum' (a:b) = maximum' ((max' a (head' b)):tail' b)

tails' [] = [[]]
tails' (a:b) = take (length (a:b)) (a:b) : tails' b

union' (a:b) [] = (a:b)
union' [] [] = []
union' [] (a:b) = (a:b)
union' (a:b) (c:d) = a : union' b ((deleteAll a) (nub' (c : d)))

intersect' [] (a:b) = []
intersect' (a:b) [] = []
intersect' [] [] = []
intersect' (a:b) (c:d)
  | elem' a (c:d) == True = a : intersect' b (c:d)
  | otherwise = intersect' b (c:d)
