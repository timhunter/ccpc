
import Test.QuickCheck
import qualified Data.List as List
import qualified PriorityQueue as PQ



prop_FindMinPQ (xs :: [Int]) 
	= not (null xs) ==>  PQ.findMin  pq == minimum xs'
	where xs' = List.nub xs
	      pq = foldr (PQ.insertBy compare) PQ.empty xs'
	
prop_DeleteFindMinPQ (xs :: [Int]) 
	= length xs' >= 2 ==>  PQ.findMin (PQ.deleteMinBy compare pq) ==  minimum xs''
	where xs' = List.nub xs
	      pq = foldr (PQ.insertBy compare) PQ.empty xs'
	      xs'' = List.delete (minimum xs') xs'
	      
prop_HeapSort (xs :: [Int])
	= not (null xs) ==> List.sort xs == PQ.toList (PQ.fromList xs)
	
prop_meld xs (ys :: [Int])
	= not (null xs) && not (null ys) ==> 
		PQ.toList (PQ.meld (PQ.fromList xs) (PQ.fromList ys)) == List.sort (xs ++ ys)