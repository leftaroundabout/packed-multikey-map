-- |
-- Module      : Data.Map.Multikey.Packed
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UnicodeSyntax            #-}

module Data.Map.Multikey.Packed
           ( -- * Multikey maps
             CMap
           , empty
           , fromList'
           , toList
           , lookup
             -- * The class of supported keys
           , Keys(..), KeyKey )
           where

import Prelude hiding (lookup)

import qualified Data.Vector as Arr
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Constraint

import Control.Monad
import Control.Monad.Trans.State
import Control.Arrow (first, second)
import Data.Function (on)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)

import Data.Ratio (Ratio)
import Data.Word
import Data.Int
import Data.Void
#if MIN_VERSION_base(4,8,0)
import Numeric.Natural
#endif

import qualified Test.QuickCheck as QC



type KeyKey k = Either ( ( k -> (LSubkey k, RSubkey k)
                         , LSubkey k -> RSubkey k -> k )
                       , (Dict (Keys (LSubkey k), Keys (RSubkey k))) )
                       (Dict (Ord k))

class Eq k => Keys k where
  type LSubkey k :: *
  type LSubkey k = k
  type RSubkey k :: *
  type RSubkey k = k
  useKeys :: KeyKey k
  useKeys' :: proxy k -> KeyKey k
  useKeys' _ = useKeys

instance Keys Void where
  useKeys = Right Dict
instance Keys () where
  useKeys = Right Dict

instance Keys Int where
  useKeys = Right Dict
instance Keys Int8 where
  useKeys = Right Dict
instance Keys Int16 where
  useKeys = Right Dict
instance Keys Int32 where
  useKeys = Right Dict
instance Keys Int64 where
  useKeys = Right Dict
instance Keys Integer where
  useKeys = Right Dict

instance Keys Word where
  useKeys = Right Dict
instance Keys Word8 where
  useKeys = Right Dict
instance Keys Word16 where
  useKeys = Right Dict
instance Keys Word32 where
  useKeys = Right Dict
instance Keys Word64 where
  useKeys = Right Dict
#if MIN_VERSION_base(4,8,0)
instance Keys Natural where
  useKeys = Right Dict
#endif

instance Keys Float where
  useKeys = Right Dict
instance Keys Double where
  useKeys = Right Dict
instance Integral a => Keys (Ratio a) where
  useKeys = Right Dict

instance Keys Char where
  useKeys = Right Dict
instance Ord a => Keys [a] where
  useKeys = Right Dict
instance Ord a => Keys (Maybe a) where
  useKeys = Right Dict
instance (Ord a, Ord b) => Keys (Either a b) where
  useKeys = Right Dict

instance (Keys x, Keys y) => Keys (x,y) where
  type LSubkey (x,y) = x
  type RSubkey (x,y) = y
  useKeys = Left ((id, (,)), Dict)
instance (Keys x, Keys y, Keys z) => Keys (x,y,z) where
  type LSubkey (x,y,z) = x
  type RSubkey (x,y,z) = (y,z)
  useKeys = Left ((\(x,y,z)->(x,(y,z)), \x (y,z)->(x,y,z)), Dict)
instance (Keys x, Keys y, Keys z, Keys w) => Keys (x,y,z,w) where
  type LSubkey (x,y,z,w) = (x,y)
  type RSubkey (x,y,z,w) = (z,w)
  useKeys = Left ((\(x,y,z,w)->((x,y),(z,w)), \(x,y) (z,w)->(x,y,z,w)), Dict)


-- | For keys of types like 'Int' or 'String', this container behaves just
--   like 'Map.Map'. But for compound types – tuples, in particular – it demands the
--   “rectangular property”: if the key @(p,x)@ is in the map and key @(q,y)@ too,
--   then keys @(q,x)@ and @(p,y)@ must be present as well.
--
-- In other words, such a map can be visualised as a /table/ with all the keys
--   on two edges and the values in the middle, like, in case of
--   @'CMap' ('Int','String') 'Double'@,
--
-- @
--      "bla"  "blub"  "foo"
--   0   3.8    4.1     2.0
--   1   3.9    6.3     6.3
--   5   1.0    11.6    2.2
--  43   54.1   10.0    10.1
-- @
data CMap k a
  = FlatMap (Map k Int) (Arr.Vector a)
  | MKeyMap (CMap (LSubkey k) Int) (CMap (RSubkey k) a)
  deriving (Functor, Foldable)

traverseCMap :: Applicative f => (a -> f b) -> CMap k a -> f (CMap k b)
traverseCMap f (FlatMap kks v) = FlatMap kks <$> traverse f v
traverseCMap f (MKeyMap msk lvs) = MKeyMap msk <$> traverseCMap f lvs

instance Traversable (CMap k) where
  traverse = traverseCMap

size :: CMap k a -> Int
size (FlatMap k _) = Map.size k
size (MKeyMap msk lsv) = size msk * size lsv

lookup :: Keys k => k -> CMap k a -> Maybe a
lookup = go 0
 where go :: Keys κ => Int -> κ -> CMap κ a -> Maybe a
       go pix k m = case (useKeys, m) of
        (Left ((splitKey,_), Dict), MKeyMap msk lsv)
          | (x,y) <- splitKey k
          , Just ix <- (pix * size msk +) <$> lookup x msk
           -> go ix y lsv
        (Right Dict, FlatMap kks v)
          | Just i <- (pix * Map.size kks +) <$> Map.lookup k kks
          , i <- Arr.length v
           -> Just $ v Arr.! i
        _ -> Nothing

flatFromList :: Ord k => [(k, a)] -> CMap k a
flatFromList l = FlatMap kks v
 where kks = fst <$> vsm
       vsm = Map.fromAscList [ (k, (i,x))
                             | (i,(k,x):_) <- zip [0..] .
                                   groupBy ((==)`on`fst) $ sortBy (comparing fst) l ]
       v = Arr.fromList $ snd . snd <$> Map.toList vsm

overIndex :: Int -> CMap k a -> CMap k a
overIndex pix (FlatMap kks v) = FlatMap kks $ Arr.drop (pix * Map.size kks) v
overIndex pix (MKeyMap msk lsv) = MKeyMap msk $ overIndex (pix * size msk) lsv

toList :: Keys k => CMap k a -> [(k,a)]
toList (FlatMap kks v) = second (v Arr.!) <$> Map.toList kks
toList (MKeyMap msk lsv) = case useKeys of
        Left ((_, combineKeys), Dict)
           -> [ (combineKeys lk rk, x)
              | (lk, pix) <- toList msk
              , (rk, x) <- toList $ overIndex pix lsv ]

data KeyStructure k
   = FlatKey (Map k ())
   | MultiKey (KeyStructure (LSubkey k)) (KeyStructure (RSubkey k))
instance ∀ k . (Keys k) => Eq (KeyStructure k) where
  x == y = case (useKeys, (x, y)) of
    ( Left ( (_ :: k -> (LSubkey k, RSubkey k), _), Dict )
     ,(MultiKey lx rx, MultiKey ly ry))
      -> lx == ly && rx == ry
    (Right Dict, (FlatKey kx, FlatKey ky))
      -> kx == ky

keyStructure :: CMap k a -> KeyStructure k
keyStructure (FlatMap k _) = FlatKey $ const () <$> k
keyStructure (MKeyMap msk lsv) = MultiKey (keyStructure msk) (keyStructure lsv)

empty :: ∀ k a . Keys k => CMap k a
empty = case useKeys' ([]::[k]) of
    Left (_, Dict) -> MKeyMap empty empty
    Right _ -> FlatMap Map.empty Arr.empty

perfectConcat :: ∀ k a . Keys k => [CMap k a] -> Maybe (CMap k a)
perfectConcat [] = Just empty
perfectConcat ms@(_:_)
  | allEq $ keyStructure <$> ms  = Just $ cat ms
  | otherwise                    = Nothing
 where cat :: ∀ κ . [CMap κ a] -> CMap κ a
       cat (MKeyMap msk lsv : qs)
           = MKeyMap msk . cat $ lsv : map (\(MKeyMap _ lsw) -> lsw) qs
       cat (FlatMap kks v : qs)
           = FlatMap kks . Arr.concat $ v : map (\(FlatMap _ w) -> w) qs

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq (x:xs) = all (==x) xs

indices :: Traversable t => t a -> t Int
indices q = (`evalState`0) . forM q $ \_ -> state $ \i -> (i,i+1)

-- | Build a map, if the given keys fulfill the /rectangular property/.
fromList' :: ∀ k a . Keys k => [(k, a)] -> Maybe (CMap k a)
fromList' l = case useKeys of
   Left ((splitKey,_), Dict) -> do
       let (lKeys, rKeys) = unzip [ ((kl,()), (kr,x))
                                  | (k,x) <- l
                                  , let (kl,kr) = splitKey k ]
       msk :: CMap (LSubkey k) () <- fromList' lKeys
       lsv <- forM (toList msk) $ \(i,())
                 -> fromList' $ first (snd . splitKey)
                            <$> filter ((==i) . fst . splitKey . fst) l
       MKeyMap (indices msk) <$> perfectConcat lsv
   Right Dict -> Just $ flatFromList l

fromFlatMap :: ∀ k a . (Keys k, Ord k) => Map k a -> CMap k a
fromFlatMap μπ = case fromList' $ Map.toList μπ of
                   Just ζπ -> ζπ

flatArb :: (Keys k, Ord k, QC.Arbitrary k, QC.Arbitrary v) => QC.Gen (CMap k v)
flatArb = fromFlatMap <$> QC.arbitrary

splitArb :: ( Keys k
            , QC.Arbitrary (CMap (LSubkey k) (Int->v))
            , QC.Arbitrary (CMap (RSubkey k) Int) )
              => QC.Gen (CMap k v)
splitArb = case useKeys of
  Left ((_, combineKeys), Dict) -> do
      lmargin <- QC.arbitrary
      rmargin <- QC.arbitrary
      let Just m = fromList' [ (combineKeys k l, f (x::Int))
                             | (k,f) <- toList lmargin
                             , (l,x) <- toList rmargin ]
      return m

instance QC.Arbitrary v => QC.Arbitrary (CMap Int v) where arbitrary = flatArb
instance QC.Arbitrary v => QC.Arbitrary (CMap Integer v) where arbitrary = flatArb
instance QC.Arbitrary v => QC.Arbitrary (CMap Double v) where arbitrary = flatArb
instance QC.Arbitrary v => QC.Arbitrary (CMap Char v) where arbitrary = flatArb
instance (QC.Arbitrary c, Ord c, QC.Arbitrary v) => QC.Arbitrary (CMap [c] v) where arbitrary = flatArb

type SplArb k l v = (QC.Arbitrary (CMap k (Int->v)), QC.Arbitrary (CMap l Int))
instance (Keys k, Keys l, SplArb k l v) => QC.Arbitrary (CMap (k,l) v) where
  arbitrary = splitArb
instance (Keys k, Keys l, Keys m, SplArb k (l,m) v) => QC.Arbitrary (CMap (k,l,m) v) where
  arbitrary = splitArb
instance (Keys k, Keys l, Keys m, Keys n, SplArb (k,l) (m,n) v) => QC.Arbitrary (CMap (k,l,m,n) v) where
  arbitrary = splitArb


instance (Show k, Keys k, Show a) => Show (CMap k a) where
  showsPrec p m = showParen (p>9) $ ("fromList' "++) . showsPrec 11 (toList m)

instance ∀ k a . (Keys k, Eq a) => Eq (CMap k a) where
  FlatMap k v == FlatMap i w = k==i && v==w
  MKeyMap k v == MKeyMap i w = case useKeys' ([]::[k]) of
   Left (_, Dict) -> k==i && v==w
