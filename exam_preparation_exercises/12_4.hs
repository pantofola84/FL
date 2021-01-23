import Control.Applicative

--Make the type ZipList an instance of Functor and Applicative Functor
newtype Ziplist a = Z [a] deriving Show

instance Functor Ziplist where
         --fmap :: (a -> b) -> ZipList a -> ZipList b
         fmap f (Z xs) = Z [f x | x <- xs]


applyOnCorrespondingValue :: [a -> b] -> [a] -> [b]
applyOnCorrespondingValue [] _ = []
applyOnCorrespondingValue _ [] = []
applyOnCorrespondingValue (f: fs) (x: xs) = [f x] ++ applyOnCorrespondingValue fs xs

instance Applicative Ziplist where
         -- pure :: a -> ZipList a
         pure v = Z (repeat v)
         -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
         (Z fs) <*> (Z xs) = Z (applyOnCorrespondingValue fs xs)
