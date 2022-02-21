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

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
    Identity a >>= f = f a

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

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c = Three mempty mempty c
    Three a' b' f <*> Three a b c = Three (a' <> a) (b' <> b) (f c)

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = liftM3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
    pure b = Three' mempty b b
    Three' a' f g <*> Three' a b c = Three' (a' <> a) (f b) (g c)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
    (=-=) = eq

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure d = Four mempty mempty mempty d
    Four a' b' c' f <*> Four a b c d = Four (a' <> a) (b' <> b) (c' <> c) (f d)

data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
    pure d = Four' mempty mempty mempty d
    Four' a' b' c' f <*> Four' a b c d = Four' (a' <> a) (b' <> b) (c' <> c) (f d)

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

instance Monad List where
    Nil >>= _ = Nil
    Cons x xs >>= f = append (f x) (xs >>= f)

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

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs) -- TODO: Guard for negative n.

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = liftM ZipList' arbitrary

instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

instance Applicative ZipList' where
    pure a = ZipList' $ repeat' a
    ZipList' Nil <*> _ = ZipList' Nil
    _ <*> ZipList' Nil = ZipList' Nil
    ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
        ZipList' (Cons (f x) tail)
            where (ZipList' tail) = (ZipList' fs) <*> (ZipList' xs)

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = frequency [(1, liftM Failure' arbitrary), (1, liftM Success' arbitrary)]

instance Functor (Validation e) where
    fmap _ (Failure' e) = Failure' e
    fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
    pure a = Success' a
    Success' f <*> Success' a = Success' (f a)
    Failure' e <*> Success' _ = Failure' e
    Success' _ <*> Failure' e = Failure' e
    Failure' e <*> Failure' e' = Failure' (e <> e')

data Nope a = NopeDotJpg deriving (Eq, Show)

instance EqProp (Nope a) where
    (=-=) = eq

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
    NopeDotJpg >>= _ = NopeDotJpg
