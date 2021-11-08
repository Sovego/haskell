
data Number = Zero | Succ Number deriving (Eq,Ord, Show)

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

div1 Zero x= Zero
div1 x Zero= Zero
div1 x (Succ Zero)=x
div1 x y| x==y = (Succ Zero) 
         | otherwise = div1' x y Zero where div1' x y count|x==y=(Succ count)
                                                                              |mns x y==Zero=count
                                                                              |otherwise= div1' (mns x y) y (Succ count)


pred1 y x | (mlt $ div1 y x) y == x = True 
         | otherwise =False 