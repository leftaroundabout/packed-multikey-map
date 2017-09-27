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
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UnicodeSyntax            #-}

module Data.Map.Multikey.Packed
           ( Keys(..), KeyKey
           , CMap
           , fromList'
           , toList )
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

perfectConcat :: ∀ k a . Keys k => [CMap k a] -> Maybe (CMap k a)
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
