
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
mod1 Zero x= Zero
mod1 x Zero= Zero
mod1 x (Succ Zero)=x
mod1 x y| x==y = (Succ Zero) 
         | otherwise = mod1' x y Zero where mod1' x y count|x==y=(Succ count)
                                                                              |mns x y==Zero=x
                                                                              |otherwise= mod1' (mns x y) y (Succ count)

