implementation module Map

import StdEnv
import Maybe

:: Map k v	= MNode (Map k v) k Int v (Map k v)
			| MLeaf

//Create function
empty :: .(Map k v)
empty = MLeaf

//Insert function
put :: k v (Map k v) -> (Map k v) | Eq k & Ord k
put k v MLeaf	= MNode MLeaf k 1 v MLeaf
put k v (MNode left nk h nv right) 
	| k == nk	= (MNode left k h v right)
	| k < nk	= balance (MNode newleft nk h nv right) //Add to left subtree and rebalance
				  with
					newleft		= (put k v left)
					h			= (max (height newleft) (height right)) + 1

				= balance (MNode left nk h nv newright) //Add to right subtree and rebalance
				  with
					newright	= (put k v right)
					h			= (max (height left) (height newright)) + 1

//Delete function
del :: k (Map k v) -> (Map k v) | Eq k & Ord k
del k MLeaf = MLeaf										//Do nothing
del k (MNode MLeaf nk h nv MLeaf)						//A node with just leaves as children can be safely removed
	| k == nk	= MLeaf	
				= (MNode MLeaf nk h nv MLeaf)
del k (MNode MLeaf nk h nv right)						//A node without smaller items
	| k == nk	= right									//When found, just remove
	| k < nk	= (MNode MLeaf nk h nv right)			//Do nothing, k is not in the mapping
				= balance (MNode MLeaf nk h nv newright)
				  with
					newright	= del k right
					h			= (height newright) + 1
del k (MNode left nk h nv MLeaf)						//A node without larger items
	| k == nk	= left									//When found just remove
	| k < nk	= balance (MNode newleft nk h nv MLeaf)
				  with
					newleft		= del k left
					h			= (height newleft) + 1
				= (MNode left nk h nv MLeaf)			//Do nothing, k is not in hte mapping
del k (MNode left nk h nv right)						//A node with both larger and smaller items
	| k == nk	= balance (MNode newleft k h v right)	//Replace with the largest of the smaller items and rebalance
				  with
					(newleft,k,v)	= takeMax left
					h				= (max (height newleft) (height right)) + 1
	| k < nk	= balance (MNode newleft nk h nv right)
				  with
					newleft 	= del k left
					h 			= (max (height newleft) (height right)) + 1
				= balance (MNode left nk h nv newright)
				  with
					newright	= del k right
					h			= (max (height left) (height newright)) + 1
where
	//Takes the k and v values from the maximum node in the tree and removes that node
	takeMax :: (Map k v) -> (Map k v, k, v)
	takeMax MLeaf = abort "takeMax of leaf evaluated" 
	takeMax (MNode left nk _ nv MLeaf)	= (left, nk, nv)
	takeMax (MNode left nk _ nv right)	= (balance (MNode left nk h nv newright), k, v)
										  with
											(newright,k,v) = takeMax right
											h = (max (height left) (height newright)) + 1

//Lookup function
get :: k (Map k v) -> (Maybe v,(Map k v)) | Eq k & Ord k
get k MLeaf = (Nothing, MLeaf)
get k (MNode left nk h nv right)
	| k == nk	= (Just nv, MNode left nk h nv right)
	| k < nk	= (mbv, MNode newleft nk h nv right)
				  with
					(mbv,newleft) = get k left
				= (mbv, MNode left nk h nv newright)
				  with
					(mbv,newright) = get k right

//Conversion functions
toList :: (Map k v) -> [(k,v)] 
toList m = toList` m []
where
	toList` MLeaf c = c
	toList` (MNode left k h v right) c = toList` left [(k,v): toList` right c]

fromList :: [(k,v)] -> (Map k v) | Eq k & Ord k
fromList list = foldr (\(k,v) t -> put k v t) empty list

//Helper functions

//Determine the height of a tree
//This information is stored inside the tree to prevent complete traversals of the tree
height :: (Map k v) -> Int
height MLeaf				= 0
height (MNode _ _ h _ _)	= h

//Balance a tree locally (E.g. not recursive. only inspect and rearrange the top of the tree)
balance :: (Map k v) -> (Map k v)
balance MLeaf = MLeaf
balance (MNode left k h v right)
	| balanceFactor < -1	
		| leftDeepest left	= leftleftRotate (MNode left k h v right)	//Left-left rotate
							= leftrightRotate (MNode left k h v right)	//Left-right rotate
	| balanceFactor > 1		
		| leftDeepest right	= rightleftRotate (MNode left k h v right)	//Right-left rotate
							= rightrightRotate (MNode left k h v right) //Right-right rotate
	| otherwise
							= MNode left k h v right //Already balanced
where
	balanceFactor = (height right) - (height left)

	leftDeepest MLeaf = False
	leftDeepest (MNode left _ _ _ right) = (height left) > (height right)

	leftleftRotate (MNode (MNode (MNode d xk xh xv c) pk _ pv b ) rk _ rv a)
		= MNode (MNode d xk xh xv c) pk ph pv (MNode b rk rh rv a)
	where
		rh = (max (height b) (height a)) + 1
		ph = (max xh rh) + 1
	leftleftRotate node = node

	leftrightRotate (MNode (MNode b rk _ rv (MNode c pk _ pv d)) xk _ xv a)
		= MNode (MNode b rk rh rv c) pk ph pv (MNode d xk xh xv a)
	where
		rh = (max (height b) (height c)) + 1
		xh = (max (height d) (height a)) + 1
		ph = (max rh xh) + 1
	leftrightRotate node = node

	rightleftRotate (MNode a xk _ xv (MNode (MNode d pk _ pv c) rk _ rv b))
		= MNode (MNode a xk xh xv d) pk ph pv (MNode c rk rh rv b)
	where
		xh = (max (height a) (height d)) + 1
		rh = (max (height c) (height b)) + 1
		ph = (max xh rh) + 1
	rightleftRotate node = node

	rightrightRotate (MNode a rk _ rv (MNode b pk _ pv (MNode c xk xh xv d)))
		= MNode (MNode a rk rh rv b) pk ph pv (MNode c xk xh xv d)
	where
		rh = (max (height a) (height b)) + 1
		ph = (max rh xh) + 1
	rightrightRotate node = node
