{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Char (digitToInt)
data Player = X | O deriving (Eq)
data Cell = Empty | Mark Player deriving (Eq)


data Field = Field [Cell]

instance Show Cell where
    show (Mark X) = "X" 
    show (Mark O) = "O"
    show (Empty) = " "

data GameState = GameState {
    player::Player,
    field::Field
    }

instance Show Field where 
    show (Field st) = show (st !! 0 ) ++ " | " ++ show (st !! 1) ++ show(st !! 2) ++" | \n" ++
                      show (st !! 3 ) ++ " | " ++ show (st !! 4) ++ show(st !! 5) ++" | \n" ++
                      show (st !! 6 ) ++ " | " ++ show (st !! 7) ++ show(st !! 8) ++" | \n"
initGame = GameState X (Field (take 9 $ repeat Empty))


insert _ _ (GameState player (Field field)) 9 = GameState player (Field field)
insert x y (GameState player(Field (cell:field))) count | (count == (x*y +(y-1))) && cell == Empty = insert x y (GameState player (Field (field ++ [Mark player]))) (count+1)
                              | otherwise = insert x y (GameState player (Field (field ++ [cell]))) (count+1)

makeTurn string (GameState player field)   | player == X = insert x y (GameState player field) 0
                                           | player == O = insert x y (GameState player field) 0              
            where x | (string !! 0) == 'A' = 0
                    | (string !! 0) == 'B' = 1
                    | (string !! 0) == 'C' = 2
                  y = digitToInt(string !! 1)

