
data Number = Zero | Succ Number deriving (Eq, Show)

inc :: Number -> Number
inc = Succ

dec :: Number -> Number
dec Zero = Zero
dec ( Succ a ) = a

mns :: Number -> Number -> Number
mns a b | b==Succ Zero = dec a
        | b/=Zero = dec $ mns a $ dec b
        | otherwise  = a

pls Zero x = x
pls x Zero =x
pls x (Succ y) = pls (inc x) y

mlt a b = mlt1 a b Zero where mlt1 a b n | n==a = dec b
                                         | otherwise = mlt1 a (pls a b) (inc n)

mydiv Zero x= Zero
mydiv x Zero= Zero
mydiv x (Succ Zero)=x
mydiv x y| x==y = (Succ Zero) 
         | otherwise = mydiv' x y Zero where mydiv' x y count|x==y=(Succ count)
                                                                              |mns x y==Zero=count
                                                                              |otherwise= mydiv' (mns x y) y (Succ count)


pred1 y x | (mlt $ mydiv y x) y == x = True 
          | otherwise =False 

nd x = nd1 x (Succ(Succ Zero)) (Zero) where nd1 x y n | y == x = n  
                                                     | pred1 y x  = nd1 x (Succ y) (Succ n)                                           
                                                     | otherwise = nd1 x (Succ y) n