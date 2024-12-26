-- Just implement tree 
data Tree a =
      Empty
    | Node (Tree a) a (Tree a)
  deriving (Show)

insert :: Ord a => Tree a -> a -> Tree a
insert Empty x = Node Empty x Empty
insert (Node l v r) x
  | x < v     = Node (insert l x) v r
  | otherwise = Node l v (insert r x)

contains :: Ord a => Tree a -> a -> Bool
contains Empty _ = False
contains (Node l v r) x
  | x == v    = True
  | x < v     = contains l x
  | otherwise = contains r x


-- define map for binary trees
map_tree :: (a -> b) -> Tree a -> Tree b --the type
map_tree _ Empty = Empty
map_tree func (Node l v r) = Node (map_tree func l) (func v) (map_tree func r)

--define fold for binary trees
--how many different folds can you come up with?
--      3 preorder inorder postorder
fold_preorder :: (b -> a -> b) -> b -> Tree a -> b --the type
fold_preorder _ acc Empty = acc
fold_preorder func acc (Node l v r) = fold_preorder func (fold_preorder func (func acc v) l) r -- ทำ v ก่อนแล้วไป l ไป r

fold_inorder :: (b -> a -> b) -> b -> Tree a -> b --the type
fold_inorder _ acc Empty = acc
fold_inorder func acc (Node l v r) = fold_inorder func (func (fold_inorder func acc l) v) r -- ทำ l ก่อนแล้วไป v ไป r

fold_postorder :: (b -> a -> b) -> b -> Tree a -> b --the type
fold_postorder _ acc Empty = acc
fold_postorder func acc (Node l v r) = func (fold_postorder func (fold_postorder func acc l) r) v -- ทำ l ก่อนแล้วไป r ไป v


--write a function height that returns the heights of a given binary treeheight of the empty tree is 0
height :: Tree a -> Int
height Empty = 0
height (Node l _ r) = 1 + max (height l) (height r)

--write a function isBST that takes a binary tree, and determine if it is a binary search tree
getRoot :: (Ord a) => Tree a -> Maybe a
getRoot Empty = Nothing
getRoot (Node _ v _) = Just v

isBST :: (Ord a) => Tree a -> Bool --the type
isBST Empty = True
isBST (Node l v r) = 
    (case getRoot l of
        Nothing -> True  
        Just lv -> v > lv)
    &&
    (case getRoot r of
        Nothing -> True 
        Just rv -> v < rv) 
    && isBST l && isBST r 

--define type NAryTree for n-ary trees
data NAryTree a = EmptyNT | NodeNT a [NAryTree a]
  deriving (Show, Eq)

--implement preorder and postorder traversals
preorderNAry :: NAryTree a -> [a]
preorderNAry EmptyNT = []
preorderNAry (NodeNT r children) = r : foldr (++) [] (map preorderNAry children)

postorderNAry :: NAryTree a -> [a]
postorderNAry EmptyNT = []
postorderNAry (NodeNT r children) = foldr (++) [] (map postorderNAry children) ++ [r]