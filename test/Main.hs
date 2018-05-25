{-# LANGUAGE KindSignatures #-}

module Main (main) where

import Prelude hiding (Either(..), either)

import Control.Applicative
import Control.Monad (liftM)
import Data.Functor.Classes
import Data.Either.Unpacked (Either(Left,Right), left, right)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup((<>)))
import Test.QuickCheck.Classes
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

main :: IO ()
main = lawsCheckMany myClassTests

myClassTests :: [(String, [Laws])]
myClassTests =
  [ ("Ground types", myLaws eitherProxy)
  , ("Higher-kinded types", myLaws1 eitherProxy1)
  ]

myLaws
  :: (Arbitrary a, Eq a, Ord a, Show a, Read a)
  => Proxy a -> [Laws]
myLaws p =
  [ eqLaws p
  , ordLaws p
  , showReadLaws p
  ]

myLaws1
  :: (Arbitrary1 a, Monad a, Functor a, Applicative a, Foldable a, Traversable a, Eq1 a, Show1 a)
  => Proxy a -> [Laws]
myLaws1 p =
  [ monadLaws p
  , functorLaws p
  , applicativeLaws p
  , foldableLaws p
  , traversableLaws p
  ]

eitherProxy2 :: Proxy Either
eitherProxy2 = Proxy

eitherProxy1 :: Proxy (Either Int)
eitherProxy1 = Proxy

eitherProxy  :: Proxy (Either Int Int)
eitherProxy  = Proxy

instance Semigroup Int where
  (<>) = (+)

instance Monoid Int where
  mempty = 0
  mappend = (+)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = arbitrary2
  shrink = shrink2

instance Arbitrary2 Either where
  liftArbitrary2 arbA arbB = oneof [liftM Left arbA, liftM Right arbB]

  liftShrink2 shrA _ (Left x)  = [ Left  x' | x' <- shrA x ]
  liftShrink2 _ shrB (Right y) = [ Right y' | y' <- shrB y ]

instance Arbitrary a => Arbitrary1 (Either a) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink
