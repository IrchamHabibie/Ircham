-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
module One where

import Data.List

-- 1.a

null' [] = True
null' x = False

--pembatas

take' n (a:b)
  | b == [] = []
  | n <= 0 = []
  | otherwise = a : take' (n-1) b

--pembatas

drop' n (a:b)
  | b == [] = []
  | n <= 0 = (a:b)
  | otherwise = drop' (n-1) b

--pembatas

map' n [] = []
map' n (a:b) = (n a) : (map' n b)

--pembatas

filter' n [] = []
filter' n (a:b)
  | (n a) == True = a : filter' n b
  | (n a) == False = filter' n b

--pembatas

fst' (x,y) = x

--pembatas

snd' (x,y) = y

--pembatas

delete' n [] = []
delete' n (a:b)
  | a == n = [] ++ b
  | otherwise = [a] ++ delete' n b

--pembatas

deleteAll n [] = []
deleteAll n (a:b)
  | a == n = deleteAll n b
  | otherwise = [a] ++ deleteAll n b

--pembatas

foldl'' x c [] = c
foldl'' x c (a:b) = foldl'' x (x c a) b

--pembatas

foldl1'' x [] = 1
foldl1'' x (a:b) = x  (foldl1'' x b) a

--pembatas

zip' [] (a:b) = []
zip' (a:b) [] = []
zip' [] [] = []
zip' (a:b) (c:d) = (a,c) : zip' (b) (d)

--pembatas

zipWith' x [] (a:b) = []
zipWith' x (a:b) [] = []
zipWith' x [] [] = []
zipWith' x (a:b) (c:d) = (x a c) : zipWith' x (b) (d)

--pembatas

nth (a:b) 0 = a
nth (a:b) x = nth b (x-1)

--pembatas

scanl'' a b [] = [b]
scanl'' a b (c:d) = [b] ++ scanl'' a (a b c) d

--pembatas

scanl1' a [] = []
scanl1' a [b] = [b]
scanl1' a (b:c) = [b] ++ scanl1' a ((a b (head' c)) : tail' (c))

--pembatas

elem' z [] = False
elem' z (a:b)
  | z == a = True
  | z /= a = elem' z b

--pembatas

notElem' z [] = True
notElem' z (a:b)
  | z == a = False
  | z /= a = notElem' z b

--pembatas

head' (a:b) = a

--pembatas

length' [] = 0
length' (a:b) = 1 + (length' b)

--pembatas

reverse' [] = []
reverse' (a:b)
  | otherwise = reverse' b ++ [a]

--pembatas

last' (a:b)
  | b == [] = a
  | otherwise = last' b

--pembatas

tail' (a:b) = b

--pembatas

init' [z] = []
init' (a:b)
  | otherwise = a : init' b

--pembatas

max' x y
  | (x >= y) = x
  | (y >= x) = y

--pembatas

min' x y
  | (x <= y) = x
  | (y <= x) = y

--pembatas

concat' [] = []
concat' [z] = z
concat' (a:b) = a ++ concat' b

--pembatas

intersperse' x [] = []
intersperse' x (a:b)
  | b == [] = a : intersperse' x b
  | otherwise = a : x : intersperse' x b

--pembatas

intercalate' (s:d) [] = []
intercalate' (s:d) (a :b)
  | b == [] = a ++ intercalate' (s:d) b
  | otherwise = a ++ (s:d) ++ intercalate' (s:d) b

--pembatas

and' [True] = True
and' (a:b)
  | a == False = False
  | a == True = and' b

--pembatas

or' [False] = False
or' (a:b)
  | a == True = True
  | a == False = or' b

--pembatas

zip3' [] [] (a:b) = []
zip3' [] (a:b) [] = []
zip3' (a:b) [] [] = []
zip3' (a:b) (c:d) [] = []
zip3' (a:b) [] (c:d) = []
zip3' [] (a:b) (c:d) = []
zip3' [] [] [] = []
zip3' (a:b) (c:d) (e:f) = (a,c,e) : zip3' (b) (d) (f)

--pembatas

sum' [] = 0
sum' (a:b) = a + (sum' b)

--pembatas

product' [] = 1
product' (a:b) = a * (product' b)

--pembatas

unlines' [] = []
unlines' (a:b) = a ++ "\n" ++ unlines' b

--pembatas

unwords' [] = []
unwords' [a] = a
unwords' (a:b) = a ++ " " ++ unwords' b

--pembatas

takeWhile' n [] = []
takeWhile' n (a:b)
  | n a == False = []
  | otherwise = a : takeWhile' n b

--pembatas

dropWhile' n [] = []
dropWhile' n (a:b)
  | n a == True = b
  | otherwise = (a:b)

--pembatas

concatMap' s [] = []
concatMap' s (a:b) = (s a) ++ concatMap' s b

--pembatas

all' n [] = True
all' n (a:b)
  | n a == False = False
  | n a == True = all' n b

--pembatas

any' b [] = False
any' n (a:b)
  | n a == False = any' n b
  | n a == True = True

--pembatas

insert' s [] = [s]
insert' s (a:b)
  | (min s a) == s = s : a : b
  | (min s a) == a = a : insert' s b

--pembatas

zipWith3' x [] [] (a:b) = []
zipWith3' x [] (a:b) [] = []
zipWith3' x (a:b) [] [] = []
zipWith3' x (a:b) (c:d) [] = []
zipWith3' x (a:b) [] (c:d) = []
zipWith3' x [] (a:b) (c:d) = []
zipWith3' x [] [] [] = []
zipWith3' z (a:b) (c:d) (e:f) = (z a c e) : zipWith3' z (b) (d) (f)

--pembatas

-- 1.b

nub' [] = []
nub' (a:b) = a : nub' (deleteAll a b)

--pembatas

sort' [] = []
sort' (a:b) = (minimum' (a:b)) : sort' (delete' (minimum' (a:b)) (a:b) )

--pembatas

minimum' [a] = a
minimum' (a:b) = minimum' ((min' a (head' b)):tail' b)

--pembatas

maximum' [a] = a
maximum' (a:b) = maximum' ((max' a (head' b)):tail' b)

--pembatas

inits' [] = [[]]
inits' (a:b) = reverse' ( asd (a:b) )
  where asd [] = [[]]
        asd (a:b) = (a:b) :  asd (init' (a:b))

--pembatas

tails' [] = [[]]
tails' (a:b) = take' (length' (a:b)) (a:b) : tails' b

--pembatas

union' (a:b) [] = (a:b)
union' [] [] = []
union' [] (a:b) = (a:b)
union' (a:b) (c:d) = a : union' b ((deleteAll a) (nub' (c : d)))

--pembatas

intersect' [] (a:b) = []
intersect' (a:b) [] = []
intersect' [] [] = []
intersect' (a:b) (c:d)
  | elem' a (c:d) == True = a : intersect' b (c:d)
  | otherwise = intersect' b (c:d)

--pembatas
--group' (a:b) = wkwk (a:b) : group' (delete a (a:b))

--wkwk (a:b)
  --| a == head b = a : wkwk (delete a (a:b))
  --| a /= head b = a : []
-- g [1,1,1,2,3] = [1,1,1] : [2] : [3] : [[]]

--pembatas
splitAt' s (a:b) = ( take' s (a:b) , drop' s (a:b) )
antifilter' n [] = []
antifilter' n (a:b)
  | (n a) == False = a : antifilter' n b
  | (n a) == True = antifilter' n b

--pembatas

partition' s (a:b) = (filter' s (a:b), antifilter' s (a:b) )

--pembatas

replicate' 0 a = []
replicate' s a = a : replicate' (s-1) a

--pembatas

iterate' a s = s : (iterate' a (a s))

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
