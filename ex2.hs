data Number = Zero | Succ Number deriving (Eq, Show)
inc :: Number -> Number
inc a = Succ a
dec :: Number -> Number
dec Zero = Zero
dec ( Succ a ) = a


moreThan (Zero) _ = False;
moreThan _ (Zero) = True;
moreThan a b = moreThan (dec a) (dec b);

max1 a b | moreThan b a = b
        | not (moreThan b a) = a

min1 a b | not (moreThan b a) = b
        | moreThan b a = a