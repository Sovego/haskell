
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

mod4 :: Number -> Number -> Number
mod4 a b | b == Zero = Zero
        | otherwise=mod1 a b Zero where
                                mod1 a b n  | b <= Zero = n
                                            | otherwise = mod1 a (mns b a) (inc n)
div1 Zero x= Zero
div1 x Zero= Zero
div1 x (Succ Zero)=x
div1 x y| x==y = (Succ Zero) 
         | otherwise = div1' x y Zero where div1' x y count|x==y=(Succ count)
                                                                              |mns x y==Zero=count
                                                                              |otherwise= div1' (mns x y) y (Succ count)
