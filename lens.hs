{-# LANGUAGE RankNTypes #-}

import qualified Data.Functor.Identity as I
import qualified Control.Applicative as A

type Lens s a = Functor f => (a -> f a) -> (s -> f s)

{- 
    Given a lens focusing on an a inside of an s, and a 
    function from a to a, and an s,  I can give you back
    a modified s from applying the function to the focus point of the lens.
-}

-- we're mapping our modification "over" the focal point of a
over :: Lens s a -> (a -> a) -> s -> s
over ln f s = I.runIdentity $ ln (I.Identity . f) s

-- same as: Functor f => (a -> f a) -> s -> f s
view :: Lens s a -> s -> a
view ln s = A.getConst $ ln A.Const s

set :: Lens s a -> a -> s -> s
set ln x = over ln (const x)

_1 :: Functor f => (a -> f a) -> (a, b) -> f (a, b)
_1 f (x, y) = fmap (\a -> (a, y)) (f x)

data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User } deriving Show

-- Functor f => (name -> f name) -> user -> f user
nameLens :: Lens User String
nameLens f user = fmap (\newName -> user { name = newName }) (f (name user))

ageLens :: Lens User Int
ageLens f user = fmap (\newAge -> user { age = newAge }) (f (age user))

ownerLens :: Lens Project User
ownerLens f project = fmap (\newOwner -> project { owner = newOwner }) (f (owner project))

ownerNameLens :: Lens Project String
ownerNameLens = ownerLens.nameLens