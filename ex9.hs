
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

mlt a b = mlt1 (dec a) b b where mlt1 a b b1  | Zero==a = b
                                        | otherwise = mlt1 (dec a) (pls b b1) b1

div1 Zero x= Zero
div1 x Zero= Zero
div1 x (Succ Zero)=x
div1 x y| x==y = (Succ Zero) 
        | otherwise = div1' x y Zero where div1' x y count|x==y=(Succ count)
                                                          |mns x y==Zero=count
                                                          |otherwise= div1' (mns x y) y (Succ count)


pred1 y x =pred1' y x Zero  
pred1' y x n | mlt y n ==x = True 
             | n==x = False
             | otherwise = pred1' y x (inc n) 
prime x = prime1 x (Succ Zero)
prime1 x y | x == y = True
           | pred1 x y = False
           | otherwise = prime1 x (inc y)