module One where

import Data.List

drop' z [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

take' z [] = []
take' 0 xs = []
take' n (x:xs) = x : take' (n-1) xs


length' [] = 0
length' (x:xs) = 1 + (length' xs)

null' [] = True
null' x = False

fst' (x,y) = x

snd' (x,y) = y

zip' [] [] = []
zip' [x] [y] = [(x,y)]

delete' z [] = []
delete' 0 xs = xs
delete' n (x:xs) = delete' (-n) xs

sum' [] = 0
sum' (x:xs) = x + (sum' xs)
