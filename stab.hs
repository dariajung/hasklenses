{-# LANGUAGE RankNTypes #-}

import qualified Data.Functor.Identity as I
import qualified Control.Applicative as A

{-
    A lens allows us to look at a inside an s, 
    _if_ we can also replace the a with a b, 
    then we can make s into t
-}

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

data User = User { age :: Int, name :: String } deriving Show
data Project = Project { owner :: User } deriving Show

-- over :: (Functor f => (a -> f b) -> (s -> f t)) -> (a -> b) -> s -> t
over :: Lens s t a b -> (a -> b) -> s -> t
over ln f s = I.runIdentity $ ln (I.Identity . f) s

-- Functor f => (a -> f b) -> (s -> f t)) -> s -> a
view :: Lens s t a b -> s -> a
view ln s = A.getConst $ ln A.Const s

ageLens :: Lens' User Int
ageLens g (User _age _name) = fmap (\i -> User { age = i, name = _name }) (g _age) 

nameLens :: Lens' User String
nameLens g (User _age _name) = fmap (\n -> User { age = _age, name = n}) (g _name)
