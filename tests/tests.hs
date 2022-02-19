module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Exercises

type M = (Mem Int String)
type MemAssoc = M -> M -> M -> Bool
type MemIdent = M -> Bool

type ITI = Fun Int Int
type I = Identity Int
type IFunctorIdentity = I -> Bool
type IFunctorCompose = (Fun Int Int) -> (Fun Int Int) -> I -> Bool

type P = Pair Int
type PFunctorIdentity = P -> Bool
type PFunctorCompose = (Fun Int Int) -> (Fun Int Int) -> P -> Bool

main :: IO()
main = do
    hspec $ do
        describe "Exercises" $ do
            it "Property for quot and rem should hold" $ do
                property prop_quotRem
            it "Property for div and mod should hold" $ do
                property prop_divMod
            it "Property semigroupAssoc for Mem should hold" $ do
                property (semigroupAssoc :: MemAssoc)
            it "Property monoidLeftIdentify for Mem should hold" $ do
                property (monoidLeftIdentify :: MemIdent)
            it "Property monoidRightIdentity for Mem should hold" $ do
                property (monoidRightIdentity :: MemIdent)
            it "Property functorIdentity for Identity should hold" $ do
                property (functorIdentity :: IFunctorIdentity)
            it "Property functorCompose for Identity should hold" $ do
                property (functorCompose' :: IFunctorCompose)
            it "Property functorIdentity for Pair should hold" $ do
                property (functorIdentity :: PFunctorIdentity)
            it "Property functorCompose for Pair should hold" $ do
                property (functorCompose' :: PFunctorCompose)
            it "Property functorIdentity for Two should hold" $ do
                property (functorIdentity :: Two String Int -> Bool)
            it "Property functorCompose for Two should hold" $ do
                property (functorCompose' :: ITI -> ITI -> Two String Int -> Bool)
            it "Property functorIdentity for Three should hold" $ do
                property (functorIdentity :: Three String String Int -> Bool)
            it "Property functorCompose for Three should hold" $ do
                property (functorCompose' :: Fun Int Int -> Fun Int Int -> Three String String Int -> Bool)
            it "Property functorIdentity for Three' should hold" $ do
                property (functorIdentity :: Three' String Int -> Bool)
            it "Property functorCompose for Three' should hold" $ do
                property (functorCompose' :: Fun Int Int -> Fun Int Int -> Three' String Int -> Bool)
    putStrLn "Pair"
    quickBatch $ applicative (Pair ("a", "b", "c") ("a", "b", "c"))
    putStrLn "Two"
    quickBatch $ applicative (undefined :: Two String (String, String, String))
    putStrLn "Three"
    quickBatch $ applicative (undefined :: Three String String (String, String, String))
    putStrLn "Three'"
    quickBatch $ applicative (undefined :: Three' String (String, String, String))
    putStrLn "Four"
    quickBatch $ applicative (undefined :: Four String String String (String, String, String))
    putStrLn "Four'"
    quickBatch $ applicative (undefined :: Four' String (String, String, String))
    putStrLn "List"
    quickBatch $ applicative (Cons ("a", "b", "c") Nil)
    putStrLn "ZipList'"
    quickBatch $ applicative (ZipList' (Cons ("a", "b", "c") Nil))
    putStrLn "Validation"
    quickBatch $ applicative (undefined :: Validation String (String, String, String))
