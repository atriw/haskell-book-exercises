module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs = lookup 3 $ zip x y

ys = lookup 6 $ zip y z

zs = lookup 4 $ zip x y

z' n = lookup n $ zip x z

x1 = liftA2 (,) xs ys

x2 = liftA2 (,) ys zs

x3 n = (z' n, z' n)

summed = uncurry (+)

bolt = liftA2 (&&) (>3) (<8)

sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

main1 :: IO()
main1 = do
    print $ foldr (&&) True $ sequA 7
    print $ sequA $ fromMaybe 0 s'
    print $ bolt $ fromMaybe 0 ys
