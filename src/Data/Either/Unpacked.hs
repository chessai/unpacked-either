--------------------------------------------------------------------------------

-- Copyright © 2018 chessai

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of chessai nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE UnboxedSums        #-}
{-# LANGUAGE UnboxedTuples      #-}

--------------------------------------------------------------------------------

{-| This module is intended to be a drop-in replacement
    for 'Data.Either'. To shave off pointer chasing, it
    uses @'-XUnboxedSums'@ to represent the @'Either'@ type
    as two machine words that are contiguous in memory, without
    loss of expressiveness that 'Data.Either' provides.

    This library provides pattern synonyms @'Left'@ and @'Right'@
    that allow users to pattern match on an Unpacked Either
    in a familiar way.

    Functions are also provided for converting an Unpacked Either
    to the base library's Either, and vice versa.

    WARNING: This library is in alpha, and the internals
    are likely to change.
-}

module Data.Either.Unpacked
  ( Either(Either, Left, Right)
  , left
  , right
  , either
  , isLeft
  , isRight
  , lefts
  , rights
  , partitionEithers
  , fromLeft
  , fromRight
  , fromBaseEither
  , toBaseEither
  ) where

--------------------------------------------------------------------------------

import Prelude
  ()

import           Control.Applicative (Alternative((<|>)), Applicative((<*>), pure))

import           Control.Monad       (Monad(return, (>>=)))

import           Data.Bifoldable (Bifoldable(bifoldMap))
import           Data.Bifunctor (Bifunctor(bimap))
import           Data.Bitraversable (Bitraversable(bitraverse))

import           Data.Eq             (Eq((==)))
import           Data.Foldable
  (Foldable(foldMap, foldr, foldl, length, null, product, sum), foldr')

import           Data.Function       (const, id, flip, (.), ($))
import           Data.Functor        (Functor(fmap))
import           Data.Functor.Classes
  ( Eq1(liftEq)
  , Ord1(liftCompare)
  , Read1(liftReadPrec, liftReadListPrec, liftReadList)
  , Show1(liftShowsPrec)
  , readData
  , readUnaryWith
  , liftReadListPrecDefault
  , liftReadListDefault
  , showsUnaryWith
  
  , Eq2(liftEq2)
  , Ord2(liftCompare2)
  , Read2(liftReadPrec2, liftReadListPrec2, liftReadList2)
  , Show2(liftShowsPrec2) 
  , liftReadList2Default  
  , liftReadListPrec2Default  
  )

import qualified Data.Either         as BaseEither
import           Data.Monoid         (Monoid(mempty,mappend))
import           Data.Ord            (Ord(compare, (>=)), Ordering(GT, LT))
import           Data.Semigroup      (Semigroup((<>)))
import           Data.Traversable    (Traversable(sequenceA, traverse))

import           GHC.Base            (Bool(False,True))
import           GHC.Read            (Read(readPrec), expectP)
import           GHC.Show            (Show(showsPrec, showList), showString, showParen, showList__)

import           Text.Read           (parens, Lexeme(Ident), (+++), readListPrec, readListDefault, readListPrecDefault)
import qualified Text.Read           as TextRead
import           Text.ParserCombinators.ReadPrec
  (prec, step)

--------------------------------------------------------------------------------

data Either a b = Either (# a | b #)

pattern Left :: a -> Either a b
pattern Left a = Either (# a | #)

pattern Right :: b -> Either a b
pattern Right b = Either (# | b #)

{-# COMPLETE Left, Right #-}

left :: a -> Either a b
left a = Either (# a | #)
{-# INLINE left #-}

right :: b -> Either a b
right b = Either (# | b #)
{-# INLINE right #-}

either :: (a -> c) -> (b -> c) -> Either a b -> c
either fa fb (Either x) = case x of
  (# a | #) -> fa a
  (# | b #) -> fb b
{-# INLINE either #-}

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)
{-# INLINE isLeft #-}

isRight :: Either a b -> Bool
isRight = either (const False) (const True)
{-# INLINE isRight #-}

lefts :: [Either a b] -> [a]
lefts x = [a | Left a <- x]
{-# INLINEABLE lefts #-}

rights :: [Either a b] -> [b]
rights x = [b | Right b <- x]
{-# INLINEABLE rights #-}

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr' (either l r) ([],[])
  where
    l a (lft, rgt) = (a:lft, rgt)
    r a (lft, rgt) = (lft, a:rgt)

fromLeft :: a -> Either a b -> a
fromLeft def = either id (const def)
{-# INLINE fromLeft #-}

fromRight :: b -> Either a b -> b
fromRight def = either (const def) id
{-# INLINE fromRight #-}

fromBaseEither :: BaseEither.Either a b -> Either a b
fromBaseEither (BaseEither.Left  a) = left a
fromBaseEither (BaseEither.Right b) = right b

toBaseEither :: Either a b -> BaseEither.Either a b
toBaseEither (Left  a) = BaseEither.Left a
toBaseEither (Right b) = BaseEither.Right b

--------------------------------------------------------------------------------

-- this is what happens when you can't derive things
instance (Eq a, Eq b) => Eq (Either a b) where
  Left  a == Left  b = a == b
  Right a == Right b = a == b
  _       == _       = False
  {-# INLINE (==) #-}

-- this is what happens when you can't derive things
instance (Ord a, Ord b) => Ord (Either a b) where
  compare x y
    = case x of
        Left a -> case y of
          Left b -> compare a b
          _      -> LT
        Right a -> case y of
          Right b -> compare a b
          _       -> GT
  {-# INLINE compare #-}

-- this is what happens when you can't derive things
instance (Read a, Read b) => Read (Either a b) where
  readPrec
    = parens (prec 10
          (do expectP (Ident "Left")
              a <- step readPrec
              return (Left a))
          +++
            prec
              10
              (do expectP (Ident "Right")
                  b <- step readPrec
                  return (Right b)))
  readList = readListDefault
  readListPrec = readListPrecDefault

-- this is what happens when you can't derive things
instance (Show b, Show a) => Show (Either a b) where
  showsPrec i (Left a)
    = showParen
        (i >= 11)
        ((.)
           (showString "Left ") (showsPrec 11 a))
  showsPrec i (Right b)
    = showParen
        (i >= 11)
        ((.)
           (showString "Right ") (showsPrec 11 b))
  showList = showList__ (showsPrec 0)

instance Functor (Either a) where
  fmap f = either left (right . f)
  {-# INLINE fmap #-}

instance Applicative (Either e) where
  pure = right
  {-# INLINE pure #-} 
  ef <*> ex = either left (\f -> fmap f ex) ef
  {-# INLINE (<*>) #-}

instance Monad (Either e) where
  return = right
  {-# INLINE return #-}
  ex >>= f = either left f ex 
  {-# INLINE (>>=) #-}

instance Foldable (Either a) where
  foldMap f e = either (const mempty) f e
  {-# INLINE foldMap #-}
  foldr f z e = either (const z) ((flip f) z) e
  {-# INLINE foldr #-}
  foldl f z e = either (const z) (f z) e
  {-# INLINE foldl #-}
  length = either (const 0) (const 1)
  {-# INLINE length #-}
  null = isLeft
  {-# INLINE null #-}
  product = either (const 0) id
  {-# INLINE product #-}
  sum = either (const 0) id
  {-# INLINE sum #-}

instance Traversable (Either a) where
  sequenceA ea = either (pure . left) (fmap right) ea
  {-# INLINE sequenceA #-}
  traverse f ea = either (pure . left) (fmap right . f) ea 
  {-# INLINE traverse #-}

instance (Eq a) => Eq1 (Either a) where
    liftEq = liftEq2 (==)

instance (Ord a) => Ord1 (Either a) where
    liftCompare = liftCompare2 compare

instance (Read a) => Read1 (Either a) where
    liftReadPrec = liftReadPrec2 readPrec readListPrec

    liftReadListPrec = liftReadListPrecDefault
    liftReadList     = liftReadListDefault

instance (Show a) => Show1 (Either a) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq2 Either where
    liftEq2 e1 _ (Left x) (Left y) = e1 x y
    liftEq2 _ _ (Left _) (Right _) = False
    liftEq2 _ _ (Right _) (Left _) = False
    liftEq2 _ e2 (Right x) (Right y) = e2 x y

instance Ord2 Either where
    liftCompare2 comp1 _ (Left x) (Left y) = comp1 x y
    liftCompare2 _ _ (Left _) (Right _) = LT
    liftCompare2 _ _ (Right _) (Left _) = GT
    liftCompare2 _ comp2 (Right x) (Right y) = comp2 x y

instance Read2 Either where
    liftReadPrec2 rp1 _ rp2 _ = readData $
         readUnaryWith rp1 "Left" Left <|>
         readUnaryWith rp2 "Right" Right

    liftReadListPrec2 = liftReadListPrec2Default
    liftReadList2     = liftReadList2Default

instance Show2 Either where
    liftShowsPrec2 sp1 _ _ _ d (Left x) = showsUnaryWith sp1 "Left" d x
    liftShowsPrec2 _ _ sp2 _ d (Right x) = showsUnaryWith sp2 "Right" d x

instance Bifunctor Either where
  bimap f g = either (left . f) (right . g)
  {-# INLINE bimap #-}

instance Bifoldable Either where
  bifoldMap f g = either f g 
  {-# INLINE bifoldMap #-}

instance Bitraversable Either where
  bitraverse f g = either (fmap left . f) (fmap right . g) 
  {-# INLINE bitraverse #-}
