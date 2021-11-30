data ATree a = ALeaf a | ABranch (ATree a) a (ATree a) deriving (Show, Eq)

singleton x = ABranch (ALeaf 0) x (ALeaf 0)

treeInsert x (ALeaf 0) = singleton x

treeInsert x (ABranch left a right)
    | x == a = ABranch left x right
    | x < a = ABranch (treeInsert x left) a right
    | x > a = ABranch left a (treeInsert x right)


height (ALeaf _) = 1
height (ABranch left x right) = 1 + max (height left) (height right)


avgTree (ALeaf _) = 0.0 
avgTree tree =
  let   (sum1, count1) = avgTree' tree
  in    sum1 / count1
    where
      avgTree'  (ALeaf _)  =  (0,0) 
      avgTree'  (ABranch left n right) =
          let  (sumL,countL) = avgTree' left   
               (sumR,countR) = avgTree' right
          in
              ((n+sumL+sumR) , (1+countL+countR))

-- ABranch (ABranch (ALeaf 0) 4 (ABranch (ALeaf 0) 6 (ALeaf 0))) 8 (ALeaf 0)