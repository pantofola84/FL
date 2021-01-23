import Control.Applicative

-------------------------12.2-------------------------------------------------------------------------------------------

--Make the ((->) a) type a functorial type


--((->) a) is the function in which the domain is set (i.e. 'a').
--Maybe is more intuitive to think it like a box in the form "a ->"
instance Functor ((->) a) where
         --fmap :: (b -> c) -> (a -> b) -> (a -> c)
         fmap = (.)

--We leave parameters implicit and note, by observing the type of "fmap", that it is equal to the function composition o
--perator

------------------------------------------------------------------------------------------------------------------------

-----------------------12.3---------------------------------------------------------------------------------------------

--Make the type ((->) a) an Applicative Functor


instance Applicative ((->) a) where
         -- pure :: b -> (a -> b)
         pure v = \x -> v
         -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
         f <*> af = \x -> f x (af x)

------------------------------------------------------------------------------------------------------------------------
