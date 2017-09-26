-- |
-- Module      : Data.Vector.Unboxed.Nested
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UnicodeSyntax            #-}

module Data.Map.Multikey.Packed
           ( Keys(..)
           , CMap
           , fromList' )
           where

import Prelude hiding (lookup)

import qualified Data.Vector as Arr
import qualified Data.Map as Map
import Data.Map (Map, (!?))
import Data.Constraint

import Control.Monad
import Control.Monad.Trans.State
import Control.Arrow (first)

class Eq k => Keys k where
  type LSubkey k :: *
  type LSubkey k = k
  type RSubkey k :: *
  type RSubkey k = k
  useKeys :: Either ( k -> (LSubkey k, RSubkey k)
                    , (Dict (Keys (LSubkey k), Keys (RSubkey k))) )
                    (Dict (Ord k))

instance Keys Int where
  useKeys = Right Dict
instance Keys Double where
  useKeys = Right Dict
instance Keys Char where
  useKeys = Right Dict
instance Ord a => Keys [a] where
  useKeys = Right Dict

instance (Keys x, Keys y) => Keys (x,y) where
  type LSubkey (x,y) = x
  type RSubkey (x,y) = y
  useKeys = Left (id, Dict)


data CMap k a
  = FlatMap (Map k Int) (Arr.Vector a)
  | MKeyMap (CMap (LSubkey k) Int) (CMap (RSubkey k) a)
  deriving (Functor, Foldable, Traversable)

size :: CMap k a -> Int
size (FlatMap k _) = Map.size k
size (MKeyMap msk lsv) = size msk * size lsv

lookup :: Keys k => k -> CMap k a -> Maybe a
lookup = go 0
 where go :: Keys κ => Int -> κ -> CMap κ a -> Maybe a
       go pix k m = case (useKeys, m) of
        (Left (splitKey, Dict), MKeyMap msk lsv)
          | (x,y) <- splitKey k
          , Just ix <- (pix * size msk +) <$> lookup x msk
           -> go ix y lsv
        (Right Dict, FlatMap kks v)
          | Just i <- (pix * Map.size kks +) <$> (kks !? k)
          , i <- Arr.length v
           -> Just $ Arr.unsafeIndex v i
        _ -> Nothing

flatFromList :: Ord k => [(k, a)] -> CMap k a
flatFromList l = FlatMap kks v
 where kks = fst <$> vsm
       vsm = Map.fromList [ (k, (i,x)) | (i,(k,x)) <- zip [0..] l ]
       v = Arr.fromList $ snd . snd <$> Map.toList vsm

toList :: Keys k => CMap k a -> [(k,a)]
toList = undefined

data KeyStructure k
   = FlatKey (Map k ())
   | MultiKey (KeyStructure (LSubkey k)) (KeyStructure (RSubkey k))
instance ∀ k . (Keys k) => Eq (KeyStructure k) where
  x == y = case (useKeys, (x, y)) of
    (Left (_ :: k -> (LSubkey k, RSubkey k), Dict), (MultiKey lx rx, MultiKey ly ry))
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
indices q = (`evalState`0) . forM q $ \_ -> state $ \i -> (i,i)

fromList' :: ∀ k a . Keys k => [(k, a)] -> Maybe (CMap k a)
fromList' l = case useKeys of
   Left (splitKey, Dict) -> do
       let (lKeys, rKeys) = unzip [ ((kl,()), (kr,x))
                                  | (k,x) <- l
                                  , let (kl,kr) = splitKey k ]
       msk :: CMap (LSubkey k) () <- fromList' lKeys
       lsv <- forM (toList msk) $ \(i,())
                 -> fromList' $ first (snd . splitKey)
                            <$> filter ((==i) . fst . splitKey . fst) l
       MKeyMap (indices msk) <$> perfectConcat lsv
   Right Dict -> Just $ flatFromList l
