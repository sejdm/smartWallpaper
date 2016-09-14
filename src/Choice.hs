{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Choice (choice2maybe, Choicest (..), depends, unchoice) where
import Data.Semigroup

class Choice a where
  (<|||>) :: a -> a -> a

firstCorrect :: Choice a => [a] -> a
firstCorrect = foldr1 (<|||>)

instance Choice b => Choice (a -> b) where
  f <|||> g = \x -> f x <|||> g x

instance Choice (Maybe a) where
  Nothing <|||> x = x
  x <|||> _  = x

newtype Choicest a = Choicest {choice2maybe :: Maybe a} deriving (Functor, Applicative, Monad,  Choice, Show)

instance Monoid (Choicest a) where
  (Choicest x) `mappend` (Choicest y) = Choicest $ x <|||> y
  mempty = Choicest Nothing

instance Semigroup (Choicest a) where
  (<>) = mappend

depends (Choicest (Just x)) f _ = f x
depends (Choicest Nothing) _ n = n

unchoice _ (Choicest (Just x)) = x
unchoice a (Choicest Nothing)  = a
