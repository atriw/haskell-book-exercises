module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Control.Monad
import Control.Applicative

prop_quotRem :: Int -> (NonZero Int) -> Bool
prop_quotRem x (NonZero y) = (quot x y) * y + (rem x y) == x

prop_divMod :: Int -> (NonZero Int) -> Bool
prop_divMod x (NonZero y) = (div x y) * y + (mod x y) == x

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c =
    (a <> b) <> c == a <> (b <> c)

monoidLeftIdentify :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentify m =
    mempty <> m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m =
    m <> mempty == m

newtype Mem s a =
    Mem {
        runMem :: s -> (a, s)
    }

instance Semigroup a => Semigroup (Mem s a) where
    Mem f1 <> Mem f2 = Mem $ \s -> let a = f1 s
                                       b = f2 (snd a) in (fst a <> fst b, snd b)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

instance (Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
    arbitrary = liftM2 (\s a -> Mem $ \_ -> (a, s)) arbitrary arbitrary

instance (Bounded s, Eq a, Eq s) => Eq (Mem s a) where
    m == m' = (runMem m maxBound) == (runMem m' maxBound)

instance (Bounded s, Show a, Show s) => Show (Mem s a) where
    show m = show . runMem m $ maxBound

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x =
    (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c)) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x =
    (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = liftM Identity arbitrary

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = liftM2 Pair arbitrary arbitrary

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure a = Pair a a
    Pair f g <*> Pair a b = Pair (f a) (g b)

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = liftM2 Two arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure b = Two mempty b
    Two a' f <*> Two a b = Two (a' <> a) (f b)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = liftM3 Three arbitrary arbitrary arbitrary

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

newtype Mu f = InF { outF :: f (Mu f) }

data LiftItOut f a =
    LiftItOut (f a)
    deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b =
    IgnoreSomething (f a) (g b)
    deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance (Eq a) => EqProp (List a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil), (4, liftA2 Cons arbitrary arbitrary)]

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    Cons f fs <*> xs =
        (fmap f xs) `append` (fs <*> xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
    Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: (List (List a)) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' (fmap f xs)
