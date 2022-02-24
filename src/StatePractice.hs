module StatePractice where

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap f (Moi g) = Moi $ fmap (\(a, s) -> (f a, s)) g

instance Applicative (Moi s) where
    pure a = Moi $ \s -> (a, s)
    (Moi f) <*> (Moi g) =
        Moi $ (\s -> \(a, s') -> let (ab, s'') = f s' in (ab a, s'')) <*> g

instance Monad (Moi s) where
    (Moi f) >>= g =
        Moi $ \s -> let (a, s') = f s in runMoi (g a) $ s'

