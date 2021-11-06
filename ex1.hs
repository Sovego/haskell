data Number = Zero | Succ Number deriving (Eq, Show)
inc :: Number -> Number
inc a = Succ a
dec :: Number -> Number
dec Zero = Zero
dec ( Succ a ) = a

mlt a b = mlt1 a b Zero where mlt1 a b n | n==a = dec b
                                         | otherwise = mlt1 a (pls a b) (inc n)


mns :: Number -> Number -> Number
mns a b | b==Succ Zero = dec a
        | b/=Zero = dec $ mns a $ dec b
        | otherwise  = a

pls Zero x = x
pls x Zero =x
pls x (Succ y) = pls (inc x) y