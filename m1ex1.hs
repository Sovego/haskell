
import Prelude hiding ((!!),init,reverse,(++),cycle,take,elem)

(!!) :: (Num t, Ord t) => t -> [p] -> p

(!!) _ []  = error "Empty list"
(!!) 0 (x:_)  = x
(!!) n _ | n<=0 = error "Invalid Index"
(!!) k (_:xs) = (!!) (k-1) xs

init :: [a] -> [a]

init [] = error "Empty list"
init [x] = []
init (x:xs) = x : init xs

(++) :: [a] -> [a] -> [a]

(++) [] [] = []
(++) [] (x:xs) = x:(++) [] xs
(++) (x:xs) ys = x : (++) xs ys

cycle :: [a] -> [a]

cycle [] = []
cycle xs = xs ++ cycle xs

take :: (Ord t, Num t) => t -> [a] -> [a]

take n _ | n <= 0 =  []
take _ [] = []
take n (x:xs) =  x : take (n-1) xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)

inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = xs : inits (init xs)


elem _ [] = False

elem a (x:xs) | a == x = True 
              | otherwise = elem a xs


