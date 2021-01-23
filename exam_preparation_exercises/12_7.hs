import Control.Applicative
import Control.Monad

--Make the following type

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

--an instance of Functor, Applicative and Monad


instance Functor Expr where
         --fmap :: (a -> b) -> Expr a -> Expr b
         fmap f (Var p) = Var (f p)
         fmap f (Val n) = Val n
         fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)


instance Applicative Expr where
         -- pure :: a -> Expr a
         pure o = Var o
         -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
         (Var f) <*> ev = fmap f ev
         (Val n) <*> ev = Val n
         (Add l r) <*> ev = Add (l <*> ev) (r <*> ev)


instance Monad Expr where
         -- return :: a -> Expr a
         return = pure
         -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
         (Var v) >>= f = f v
         (Val n) >>= f = Val n
         (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)

--The bind operator (>>=) allows us to operate recursively on variables: one can provide a function that replaces every 
--instance of a variable with a certain value. Using the "do notation" makes the code very readable and very simple.
