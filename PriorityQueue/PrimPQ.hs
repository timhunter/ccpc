module PriorityQueue.PrimPQ where
-- define a primitive priority queue
type Rank = Int

newtype Tree a = Node (a, Rank, [Tree a]) deriving (Eq,Show)

type PrimPQ a = [Tree a]



root (Node (x, _, _)) = x
rank (Node (_, r, _)) = r

-- extract a less-than-or-equal function from a compare function
leq cmp x y = case cmp x y of
		LT -> True
		EQ -> True
		GT -> False

empty = []
isEmpty = null

link cmp t1@(Node (x1,r1,c1)) t2@(Node (x2,r2,c2))
	| leq cmp x1 x2 = Node (x1, r1+1, t2 : c1)
	| otherwise = Node (x2, r2+1, t1 : c2)
	
skewLink cmp t0@(Node (x0,r0,_)) t1@(Node (x1,r1,c1)) t2@(Node (x2,r2,c2))
	| leq cmp x1 x0 && leq cmp x1 x2 = Node (x1, r1+1, t0 : t2 : c1)
	| leq cmp x2 x0 && leq cmp x2 x1 = Node (x2, r2+1, t0 : t1 : c2)
	| otherwise = Node (x0, r1+1, [t1, t2])	
 
ins _ t [] = [t]
ins cmp t (t':ts)
	| rank t < rank t' = t : t' : ts
	| otherwise = ins cmp (link cmp t t') ts


insert cmp x ts@(t1:t2:rest) 
	| rank t1 == rank t2 = skewLink cmp (Node (x,0,[])) t1 t2 : rest
	| otherwise = Node (x,0,[]) : ts
insert cmp x ts = Node (x,0,[]) : ts

meld cmp ts ts' = meldUniq cmp (uniqify cmp ts) (uniqify cmp ts')

uniqify _ [] = []
uniqify cmp (t:ts) = ins cmp t ts

meldUniq _ [] ts = ts
meldUniq _ ts [] = ts
meldUniq cmp (t1:ts1) (t2:ts2)
	| rank t1 < rank t2 = t1 : meldUniq cmp ts1 (t2:ts2)
	| rank t2 < rank t1 = t2 : meldUniq cmp (t1:ts1) ts2
	| otherwise = ins cmp (link cmp t1 t2) (meldUniq cmp ts1 ts2)

findMin _ [] = error "Empty queue!"
findMin _ [t] = root t
findMin cmp (t:ts)
	| leq cmp (root t) x = root t
	| otherwise = x
	where x = findMin cmp ts
	

deleteMin _ [] = error "deleteMin not defined for empty priority queues!"
deleteMin cmp ts = foldr (insert cmp) (meld cmp ts'' ts') xs'
	where getMin [t] = (t, [])
	      getMin (t:ts) | leq cmp (root t) (root t') = (t,ts)
	      		    | otherwise = (t', t:ts')
	      		    where (t',ts') = getMin ts
	      
	      -- find the roots of all r0 trees, and all non-r0 trees
	      split ts xs [] = (ts,xs)
	      split ts xs (t:c)
	      	| rank t == 0 = split ts (root t : xs) c
	      	| otherwise = split (t:ts) xs c

	      (ts',xs') = split [] [] c 
	      (Node (x,r,c), ts'') = getMin ts
	      
	      
 		      	 
-- [ccshan 2007-09-30]

instance Functor Tree where
    fmap f (Node (a, r, ts)) = Node (f a, r, fmap (fmap f) ts)
