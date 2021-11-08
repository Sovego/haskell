

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
mod1 Zero x= Zero
mod1 x Zero= Zero
mod1 x (Succ Zero)=x
mod1 x y| x==y = (Succ Zero) 
         | otherwise = mod1' x y Zero where mod1' x y count|x==y=(Succ count)
                                                                              |mns x y==Zero=x
                                                                              |otherwise= mod1' (mns x y) y (Succ count)
gcd' Zero a =  a
gcd' a b  =  gcd' (mod1 b a) a 