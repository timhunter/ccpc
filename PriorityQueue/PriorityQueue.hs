{- 

This module implements an asymptotically optimal priority queue.
The implementation is heavily based upon the strategy detailed in the paper 
"Optimal Purely Functional Priority Queues" by Gerth StØlting Brodal and Chris Okasaki
-}

module PriorityQueue.PriorityQueue (
			PriorityQueue,
			empty,
			isEmpty,
			findMin,
			insertBy,
			insert,
			insertKV,
			meldBy,
			meld,
			meldKV,
			deleteMinBy,
			deleteMin,
			deleteMinKV,
			fromAscList,
			toAscList,
			fromListBy,
			fromList,
			fromListKV,
			toListBy,
			toList,
			singleton
		     ) where


import qualified PriorityQueue.PrimPQ as PQ
import Data.List(intersperse)

-- A rooted skewed binomial tree which contains other rooted skewed binomial trees
newtype RQ a = RQ (a, (PQ.PrimPQ (RQ a))) deriving (Eq)


-- a data type which is either empty or contains a rooted skewed binomial tree of
-- other rooted skewed binomial trees
data PriorityQueue a = Empty | NonEmpty (RQ a)


-- transforms a function comparing the values of rooted priority queues
-- into a function comparing the rooted prirority queues themselves
cmpRQ cmp (RQ (x1,_)) (RQ (x2,_)) = cmp x1 x2

keycmp :: (k -> k -> Ordering) -> ((k,a) -> (k,a) -> Ordering)
keycmp f (k1,_) (k2,_) = f k1 k2 -- use to lift a comparison functio into a "key-comparison function"

stdcmp :: (Ord k) => (k,v) -> (k,v) -> Ordering
stdcmp = keycmp compare

-- helper function to perform "<=" comparisons given an ordering function
leq cmp x y = case cmp x y of
		LT -> True
		EQ -> True
		GT -> False

-- | /O(1)/ Returns an empty priority queue
empty = Empty



-- | /O(1)/ Return a Bool indicating if a priority queue is empty
isEmpty Empty = True
isEmpty _ = False



-- | /O(1)/ Returns the minimum element in the priority queue
findMin Empty = error "Empty queue"
findMin (NonEmpty (RQ (x,q))) = x



-- | /O(1)/ Inserts an element into a priority queue
insertBy :: 	(a -> a -> Ordering) 	-- ^ An ordering function
		-> a 			-- ^ The element to insert	
		-> PriorityQueue a 	-- ^ The priority queue in which the element will be inserted
		-> PriorityQueue a	-- ^ The resulting priority queue
insertBy cmp x xs = meldBy cmp (NonEmpty (RQ (x, PQ.empty))) xs




-- | /O(1)/ Melds two priority queues
meldBy :: (a -> a -> Ordering) 	-- ^ An ordering function
          -> PriorityQueue a	-- ^ The first priority queue
          -> PriorityQueue a	-- ^ The second priority queue
          -> PriorityQueue a	-- ^ The result of melding the first and second priority queue
meldBy _ Empty xs = xs
meldBy _ xs Empty = xs
meldBy cmp (NonEmpty (r1@(RQ (x1,q1)))) (NonEmpty (r2@(RQ (x2,q2))))
	| leq cmp x1 x2 = NonEmpty (RQ (x1, PQ.insert (cmpRQ cmp) r2 q1))
	| otherwise = NonEmpty (RQ (x2, PQ.insert (cmpRQ cmp) r1 q2))




-- | /O(log n)/ Deletes the minimum element from the priority queue
deleteMinBy :: 	(a -> a -> Ordering)	-- ^ An ordering function
		-> PriorityQueue a 	-- ^ The priority queue from which the minimu element will be deleted
		-> PriorityQueue a	-- ^ The resulting priority queue
deleteMinBy _ Empty = error "deleteMinBy is not supported for empty priority queues"
deleteMinBy cmp (NonEmpty (RQ (x,q)))
	| PQ.isEmpty q = Empty
	| otherwise = NonEmpty (RQ (y, PQ.meld (cmpRQ cmp) q1 q2))
	where RQ (y,q1) = PQ.findMin (cmpRQ cmp) q 
	      q2 = PQ.deleteMin (cmpRQ cmp) q
	

-- standard interface

-- | /O(1)/ A less general version of insertBy
insert :: (Ord a) => a -> PriorityQueue a -> PriorityQueue a
insert = insertBy compare

-- | /O(1)/ A less general version of meldBy
meld :: (Ord a) => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
meld = meldBy compare

-- | /O(log n)/ A less general version of deleteMinBy
deleteMin :: (Ord a) => PriorityQueue a -> PriorityQueue a
deleteMin = deleteMinBy compare	
	      
	      
-- | Helper function for working with key-value pairs in a priority queue.
-- This inserts a key-value pair into a priority queue, using the Ord instance of the key
-- to determine ordering.	      
insertKV :: (Ord k) => (k,v) -> PriorityQueue (k,v) -> PriorityQueue (k,v)	      
insertKV (k,v) pq = insertBy stdcmp (k,v) pq



-- | Helper function for working with key-value pairs in a priority queue.
-- This deletes the minimum key-value pair from a priority queue, using the Ord instance of the key
-- to determine ordering.
deleteMinKV :: (Ord k) => PriorityQueue (k,v) -> PriorityQueue (k,v)
deleteMinKV = deleteMinBy stdcmp

-- | Helper function for working with key-value pairs in a priority queue.
-- This melds two priority queues, using the Ord instance of the key
-- to determine ordering.
meldKV :: (Ord k) => PriorityQueue (k,v) -> PriorityQueue (k,v) -> PriorityQueue (k,v)
meldKV = meldBy stdcmp

-- | /O(n)/ Create a priority queue from an association list
fromAscList :: (Ord k) => [(k,v)] -> PriorityQueue (k,v)
fromAscList = foldr insertKV empty

-- | /O(n log n)/ Retrieve the (sorted) association list from a priority queue of key-value pairs
toAscList :: (Ord k) => PriorityQueue (k,v) -> [(k,v)]
toAscList pq 
	| isEmpty pq = []
	| otherwise = findMin pq : toAscList (deleteMinKV pq)	


-- | /O(n log n)/ Builds a (sorted) list from a priority queue
toListBy :: 	(a->a->Ordering) 	-- ^ An ordering function
		-> PriorityQueue a 	-- ^ The priority queue
		-> [a]			-- ^ The resulting (sorted) list
toListBy cmp pq 
	| isEmpty pq = []
	| otherwise = findMin pq : toListBy cmp (deleteMinBy cmp pq)
	
-- | /O(n)/ Builds a priority queue from a list	
fromListBy :: 	(a->a->Ordering) 	-- ^ An ordering function
		-> [a] -> 		-- ^ The list of elements
		PriorityQueue a		-- ^ The resulting priority queue
fromListBy cmp = foldr (insertBy cmp) empty


-- | Less general version of toListBy
toList :: (Ord a) => PriorityQueue a -> [a]
toList pq 
	| isEmpty pq = []
	| otherwise = findMin pq : toList (deleteMin pq)

-- | Less general version of fromListBy		
fromList :: (Ord a) => [a] -> PriorityQueue a
fromList = foldr insert empty



instance (Show a, Ord a) => Show (PriorityQueue a) where
	show p = "{ " ++ concat (intersperse " " (map show (toList p))) ++ " }"



-- [ccshan 2007-09-30]

singleton :: a -> PriorityQueue a
singleton x = NonEmpty (RQ (x, PQ.empty))

fromListKV :: (Ord k) => [(k,v)] -> PriorityQueue (k,v)
fromListKV = fromListBy stdcmp

instance Functor RQ where
    fmap f (RQ (a, pq)) = RQ (f a, fmap (fmap (fmap f)) pq)

instance Functor PriorityQueue where
    fmap f Empty         = Empty
    fmap f (NonEmpty rq) = NonEmpty (fmap f rq)
