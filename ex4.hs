
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
mymod Zero x= Zero
mymod x Zero= Zero
mymod x (Succ Zero)=x
mymod x y| x==y = (Succ Zero) 
         | otherwise = mymod' x y Zero where mymod' x y count|x==y=(Succ count)
                                                                              |mns x y==Zero=x
                                                                              |otherwise= mymod' (mns x y) y (Succ count)

