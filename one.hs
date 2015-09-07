module One where

import Data.List

null' [] = True
null' x = False

take' z [] = []
take' 0 b = []
take' n (a:b) = a : take' (n-1) b

-- take 2 [3,4,5] = [3] : take 1 [4,5]
-- take 1 [4,5] = [4] : 0

drop' n (a:b)
  | b == [] = []
  | n == 0 = (a:b)
  | otherwise = drop' (n-1) b

-- drop 1 [1,2,3] = drop 0 [2,3]

map' n [] = []
map' n (a:b) = (n a) : (map' n b)

filter' n [] = []
filter' n (a:b)
  | (n a) == True = a : filter' n b
  | (n a) == False = filter' n b


-- filter odd [3,6,7,8,9] = (odd 3) : (filter odd [6,7])
-- filter odd [6,7] = (odd 6) : (filter odd [7])
-- filter [7] = odd 7 : filter odd []


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

-- delete' 2 [2,1,2] = [ ] ++ [1,2]
-- delete' 2 [1,2] =  [1] ++ delete 2 [2]
-- delete' 2 [2] = [] ++ []

nth (a:b) x
  | succ x == a = a
  | otherwise = nth b x

-- nth [1,2,3] 2 = nth

elem' z [] = False
elem' z (a:b)
  | z == a = True
  | z /= a = elem' z b

-- elem' 2 [1,2] = elem' 2 [2]
-- elem' 3 [1,2] = elem, 3 [2] = elem' 3 []

notElem' z [] = True
notElem' z (a:b)
  | z == a = False
  | z /= a = notElem' z b

head' (a:b) = a

length' [] = 0
length' (a:b) = 1 + (length' b)
-- length' [1,2,3] = 1 + (length' [2,3])
-- Length' [2,3] = 1 + (length' [3])
-- length' [3] = 1 + length []

reverse' [] = []
reverse' (a:b)
  | otherwise = reverse' b ++ [a]

-- reverse [1,2,3] = [1,2] : [3]
-- reverse [1,2] = [2] : [1]

last' (a:b)
  | b == [] = a
  | otherwise = last' b

tail' (a:b) = b

init' [z] = []
init' (a:b)
  | otherwise = a : init' b
-- init [1,2,3,4] = [1] ++ init [2,3,4]
-- init [2,3,4] = [2] ++ init [3,4]
-- init [3,4] = [3] ++ init [4]

max' x y
  | (x > y) = x
  | (y > x) = y

min' x y
  | (x < y) = x
  | (y < x) = y

concat' [] = []
concat' [z] = z
concat' (a:b) = a ++ concat' b

-- conc [[1,2],[1,3]] = 1,2 : conc [1,3]
-- conc [1,3] = 1,3

intersperse' x [] = []
intersperse' x (a:b)
  | b == [] = a : intersperse' x b
  | otherwise = a : x : intersperse' x b

-- int 1 [1,2,3] = 1 : 1 : int 1 [2,3]
-- int 1 [2,3] = 2 : 2 : int 1 [3]
-- int 1 [3] = 3 : [] : int 1 []


intercalate' [x] [] = []
intercalate' [x] (a:b)
  | b == [] = a ++ intercalate' [x] b
  | otherwise = a ++ [x] ++ intercalate' [x] b

-- int [1] [[1,2],[1,3],[1,4]] = 1,2 : 1 : int  b
-- int [1] [[1,3],[1,4]] = 1,3 : 1 :


-- [1,2,1,1,3]


and' [True] = True
and' (e:es)
  | e == False = False
  | e == True = and' es

or' [False] = False
or' (e:es)
  | e == True = True
  | e == False = or' es

-- zip [1,2,7] [3,4,8] [5,6,9] = [(1,3,5)] : zip [2,7] [4,8] [3,5]
-- zip [2,7] [4,8] [6,9] = [(2,4,8)] :

sum' [] = 0
sum' (e:es) = e + (sum' es)

product' [] = 1
product' (e:es) = e * (product' es)

nub' [] = []
nub' (a:b)
  | b == [] = []
  | otherwise = a : nub (deleteAll a b)
