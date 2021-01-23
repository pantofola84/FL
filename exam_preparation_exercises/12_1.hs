--Make this type a functorial type
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show


instance Functor Tree where
         --fmap :: (a -> b) -> Tree a -> Tree b
         fmap f t = case t of
                         Leaf -> Leaf
                         Node leftTree inf rightTree -> Node (fmap f leftTree) (f inf) (fmap f rightTree)
