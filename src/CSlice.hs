{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module CSlice where

import Control.Applicative
import Data.Maybe
import Data.Aeson
import qualified Data.Map.Strict as M

data Range rangetype
  = Range
  { _start :: !rangetype -- ^ inclusive bound
  , _end   :: !rangetype -- ^ inclusive bound
  }
  deriving (Show, Eq, Ord)

instance FromJSON rangetype => FromJSON (Range rangetype) where
  parseJSON = withObject "Range" $ \hm -> Range <$> hm .: "s" <*> hm .: "e"

instance ToJSON rangetype => ToJSON (Range rangetype) where
  toJSON (Range s e) = object ["s" .= s, "e" .= e]

-- | Returns true if start < end
wellFormedRange
  :: Ord t
  => Range t
  -> Bool
wellFormedRange (Range s e) = s <= e

-- | returns true if the two ranges are adjacent
--
-- It is allowed to consider the first range to always be before the second
-- range, and the ranges are well formed.
adjacent :: (Eq t, Enum t)
         => Range t
         -> Range t
         -> Bool
adjacent (Range _ e) (Range s _) = succ e == s

-- | returns true if the two ranges intersect
intersecting
  :: Ord t
  => Range t
  -> Range t
  -> Bool
intersecting (Range s1 e1) (Range s2 e2) =
    (s1 <= s2 && s2 <= e1) || (s2 <= s1 && s1 <= e2)

data Sliced rangetype itemtype
    = Sliced
    { ranges :: [(Range rangetype, itemtype)]
    , bounds :: Range rangetype
    }
    deriving (Show, Eq)

instance Functor (Sliced r) where
  fmap f (Sliced lst bnds) = Sliced [(r, f i) | (r,i) <- lst] bnds

instance (FromJSON rangetype, FromJSON itemtype)
        => FromJSON (Sliced rangetype itemtype) where
  parseJSON = withObject "Sliced" $ \hm -> Sliced <$> hm .: "rngs" <*> hm .: "bnds"

instance (ToJSON rangetype, ToJSON itemtype)
        => ToJSON (Sliced rangetype itemtype) where
  toJSON (Sliced rngs bnds) = object ["rngs" .= rngs, "bnds" .= bnds]

-- | Creates an empty sliced
mkSliced
  :: Ord rangetype
  => rangetype -- ^ lower limit of sliced
  -> rangetype -- ^ upper limit of sliced
  -> Sliced rangetype x
mkSliced lo hi = Sliced [] (Range lo hi)

-- | Inserts a range in a sliced, if it does not intersect
insertSlice
  :: Ord rangetype
  => Range rangetype
  -> x
  -> Sliced rangetype x
  -> Maybe (Sliced rangetype x)
insertSlice ir@(Range ilo ihi) e (Sliced lst rng@(Range smin smax))
  | ilo < smin || ihi > smax = Nothing
  | otherwise = fmap (\r -> Sliced r rng) (go lst)
  where
    element = (ir, e)
    go [] = Just [element]
    go lst'@((k@(Range clo _), _) : _)
      | intersecting ir k = Nothing
      | ihi < clo = Just (element : lst')
    go (e1@(r1@(Range _ c1hi), _) : e2@(r2@(Range c2lo _), _) : lst')
      | intersecting ir r1 = Nothing
      | intersecting ir r2 = Nothing
      | c1hi < ilo && ihi < c2lo = Just (e1 : element : e2 : lst')
      | c1hi > ilo = Nothing
      | ihi < c2lo = Nothing
    go (x:xs) = (x:) <$> go xs

-- | given a maximum slice width, find the first "hole" in the slice,
-- and put the given element in it.
--
-- Returns the new sliced, along with the selected range on success.
-- Fails if the sliced is full.
allocateSlice
  :: forall rangetype x. (Ord rangetype, Num rangetype)
  => rangetype -- ^ max slice size
  -> x -- ^ element type to insert
  -> Sliced rangetype x -- ^ current set
  -> Maybe (Sliced rangetype x, Range rangetype)
allocateSlice maxsize v (Sliced lst bnds@(Range smin smax)) = go smin lst
  where
    go :: rangetype
       -> [(Range rangetype, x)]
       -> Maybe (Sliced rangetype x, Range rangetype)
    go curbound [] =
      if curbound <= smax
        then let nrange = Range curbound (min smax (curbound + maxsize - 1))
             in  Just (Sliced [(nrange, v)] bnds, nrange)
        else Nothing
    go curbound (e@(Range cmin cmax, _):xs)
      | curbound < cmin =
          let nrange = Range curbound (min (cmin - 1) (curbound + maxsize - 1))
          in  Just (Sliced ((nrange, v):e:xs) bnds, nrange)
      | otherwise = do
          (Sliced lst' _, nr) <- go (cmax + 1) xs
          pure (Sliced (e:lst') bnds, nr)

-- | Finds the first slice that matches the predicate and change its
-- content. Coalesces all slices that have the same content.
alterSlice
  :: forall rangetype x. (Ord rangetype, Enum rangetype, Eq x)
  => (Range rangetype -> x -> Maybe x) -- ^ predicate for the range to alter
  -> Sliced rangetype x
  -> Sliced rangetype x
alterSlice p (Sliced lst bnds) = Sliced (go lst) bnds
  where
    merge :: (Range rangetype, x) -> (Range rangetype, x) -> Maybe (Range rangetype, x)
    merge (Range lmin lmax, lv) (Range rmin rmax, rv)
      = if succ lmax == rmin && lv == rv
          then Just (Range lmin rmax, rv)
          else Nothing
    go :: [(Range rangetype, x)] -> [(Range rangetype, x)]
    go ( prev : ((cur, cv) : next : xs) )
      | Just nv <- p cur cv =
          let ncur = (cur, nv)
          in  maybe (prev : ncur : next : xs) (++xs)
              (   (do m1 <- merge prev ncur; m2 <- merge m1 next; pure [m2])
              <|> (do m1 <- merge prev ncur; pure [m1, next])
              <|> (do m2 <- merge ncur next; pure [prev, m2])
              )
    go (l@(r1, v1) : r@(r2, v2) : xs)
      | Just nv <- p r1 v1 =
         let ncur = (r1, nv)
         in  maybe (ncur : r : xs) (:xs) (merge ncur r)
      | Just nv <- p r2 v2 =
         let ncur = (r2, nv)
         in  maybe (l : ncur : xs) (:xs) (merge l ncur)
      | otherwise = l : go (r:xs)
    go [(cur, cv)] = [(cur, fromMaybe cv (p cur cv))]
    go [] = []

-- | Removes a slice from a Sliced. Can punch a hole in an existing slice!
deleteSlice
  :: Eq rangetype
  => Range rangetype
  -> Sliced rangetype x
  -> Sliced rangetype x
deleteSlice rng (Sliced slc bnds) = Sliced (go slc) bnds
  where
    go slc' =
      case slc' of
        [] -> []
        x@(c, _):xs ->
          if rng == c
            then xs
            else x:go xs

summarySlice
  :: (Ord x, Num rangetype)
  => Sliced rangetype x
  -> M.Map x rangetype
summarySlice = M.fromListWith (+) . map (\(Range s e, x) -> (x, 1+e-s)) . ranges

