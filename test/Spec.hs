module Main where

import Test.Hspec
import Test.QuickCheck

import qualified CSlice as C

frange :: Gen (C.Range Integer)
frange = do
    s <- arbitrary
    w <- fpos
    pure (C.Range s (s+w))

-- > 0
fspos :: Gen Integer
fspos = suchThat arbitrary (> 0)

-- >= 0
fpos :: Gen Integer
fpos = suchThat arbitrary (>= 0)

rangesN :: Int -> Gen [C.Range Integer]
rangesN 0 = pure []
rangesN 1 = (:[]) <$> frange
rangesN n = do
    prange <- rangesN (n-1)
    case prange of
      (x@(C.Range s2 _):xs) -> do
        w1 <- fspos
        w2 <- fpos
        let e1 = s2 - w1
            s1 = e1 - w2
        pure (C.Range s1 e1 : x : xs)
      _ -> error "??"

wellFormed :: (Ord n, Num n)
           => [C.Range n]
           -> Bool
wellFormed lst =
    case lst of
      [] -> True
      [x] -> C.wellFormedRange x
      (p@(C.Range _ e1) : x@(C.Range s2 _) : xs) ->
        C.wellFormedRange p && e1 < s2 && wellFormed (x:xs)

insertSliceTests :: Spec
insertSliceTests = do
  it "once" $ property $ forAll frange $
    \r -> let ini = C.mkSliced (C._start r) (C._end r)
          in  fmap C.ranges (C.insertSlice r "x" ini) `shouldBe` Just [(r,"x")]
  it "twice" $ property $ forAll frange $
    \r -> let ini = C.mkSliced (C._start r) (C._end r)
          in  fmap C.ranges (C.insertSlice r "x" ini >>= C.insertSlice r "y") `shouldBe` Nothing
  it "after" $ property $ forAll (rangesN 2) $
    \[r1, r2] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r2)
            step1 <- C.insertSlice r1 "x" ini
            step2 <- C.insertSlice r2 "y" step1
            pure (C.ranges step2)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y")]
  it "before" $ property $ forAll (rangesN 2) $
    \[r1, r2] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r2)
            step1 <- C.insertSlice r2 "y" ini
            step2 <- C.insertSlice r1 "x" step1
            pure (C.ranges step2)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y")]
  it "between A" $ property $ forAll (rangesN 3) $
    \[r1, r2, r3] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r3)
            stp <- C.insertSlice r1 "x" ini
               >>= C.insertSlice r2 "y"
               >>= C.insertSlice r3 "z"
            pure (C.ranges stp)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y"), (r3, "z")]
  it "between B" $ property $ forAll (rangesN 3) $
    \[r1, r2, r3] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r3)
            stp <- C.insertSlice r2 "y" ini
               >>= C.insertSlice r3 "z"
               >>= C.insertSlice r1 "x"
            pure (C.ranges stp)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y"), (r3, "z")]
  it "between C" $ property $ forAll (rangesN 3) $
    \[r1, r2, r3] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r3)
            stp <- C.insertSlice r3 "z" ini
               >>= C.insertSlice r1 "x"
               >>= C.insertSlice r2 "y"
            pure (C.ranges stp)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y"), (r3, "z")]
  it "between D" $ property $ forAll (rangesN 3) $
    \[r1, r2, r3] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r3)
            stp <- C.insertSlice r3 "z" ini
               >>= C.insertSlice r2 "y"
               >>= C.insertSlice r1 "x"
            pure (C.ranges stp)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y"), (r3, "z")]
  it "between E" $ property $ forAll (rangesN 3) $
    \[r1, r2, r3] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r3)
            stp <- C.insertSlice r1 "x" ini
               >>= C.insertSlice r3 "z"
               >>= C.insertSlice r2 "y"
            pure (C.ranges stp)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y"), (r3, "z")]
  it "between F" $ property $ forAll (rangesN 3) $
    \[r1, r2, r3] ->
      let mr = do
            let ini = C.mkSliced (C._start r1) (C._end r3)
            stp <- C.insertSlice r2 "y" ini
               >>= C.insertSlice r1 "x"
               >>= C.insertSlice r3 "z"
            pure (C.ranges stp)
      in  mr `shouldBe` Just [(r1, "x"), (r2, "y"), (r3, "z")]

allocateSlice :: Spec
allocateSlice = do
  let ini = C.mkSliced 0 1000 :: C.Sliced Integer String
      shouldBeS x y = fmap (\(C.Sliced rngs _, r) -> (rngs, r)) x `shouldBe` Just y
  it "empty smaller" $
    let r1 = C.Range 0 99
    in  C.allocateSlice 100 "x" ini `shouldBeS`  ([(r1, "x")], r1)
  it "empty larger" $
    let r1 = C.Range 0 1000
    in  C.allocateSlice 10000 "x" ini `shouldBeS`  ([(r1, "x")], r1)
  it "one element, start, small" $
    let r1 = C.Range 0 99
        r2 = C.Range 100 199
    in  (C.insertSlice r1 "x" ini >>= C.allocateSlice 100 "y") `shouldBeS` ([(r1, "x"), (r2, "y")], r2)
  it "one element, start, large" $
    let r1 = C.Range 0 99
        r2 = C.Range 100 1000
    in  (C.insertSlice r1 "x" ini >>= C.allocateSlice 10000 "y") `shouldBeS` ([(r1, "x"), (r2, "y")], r2)
  it "one element, mid, small, filling" $
    let r1 = C.Range 100 199
        r2 = C.Range 0 99
    in  (C.insertSlice r1 "x" ini >>= C.allocateSlice 100 "y") `shouldBeS` ([(r2, "y"), (r1, "x")], r2)
  it "one element, mid, small, smaller" $
    let r1 = C.Range 100 199
        r2 = C.Range 0 49
    in  (C.insertSlice r1 "x" ini >>= C.allocateSlice 50 "y") `shouldBeS` ([(r2, "y"), (r1, "x")], r2)
  it "one element, mid, small, large!" $
    let r1 = C.Range 100 199
        r2 = C.Range 0 99
    in  (C.insertSlice r1 "x" ini >>= C.allocateSlice 5000 "y") `shouldBeS` ([(r2, "y"), (r1, "x")], r2)
  it "hole, small" $
    let r1 = C.Range 0 99
        r2 = C.Range 200 299
        r3 = C.Range 100 149
    in  (C.insertSlice r1 "x" ini
          >>= C.insertSlice r2 "y"
          >>= C.allocateSlice 50 "z")
             `shouldBeS` ([(r1, "x"), (r3, "z"), (r2, "y")], r3)
  it "hole, filling" $
    let r1 = C.Range 0 99
        r2 = C.Range 200 299
        r3 = C.Range 100 199
    in  (C.insertSlice r1 "x" ini
          >>= C.insertSlice r2 "y"
          >>= C.allocateSlice 100 "z")
             `shouldBeS` ([(r1, "x"), (r3, "z"), (r2, "y")], r3)
  it "hole, large" $
    let r1 = C.Range 0 99
        r2 = C.Range 200 299
        r3 = C.Range 100 199
    in  (C.insertSlice r1 "x" ini
          >>= C.insertSlice r2 "y"
          >>= C.allocateSlice 5000 "z")
             `shouldBeS` ([(r1, "x"), (r3, "z"), (r2, "y")], r3)


alterSlice :: Spec
alterSlice = do
  let r1 :: C.Range Integer
      r1 = C.Range 100 149
      r2 :: C.Range Integer
      r2 = C.Range 150 199
      r3 :: C.Range Integer
      r3 = C.Range 200 249
      r4 :: C.Range Integer
      r4 = C.Range 250 299
      r5 :: C.Range Integer
      r5 = C.Range 300 349
      shouldBeS x y = C.ranges x `shouldBe` y
      onS :: C.Range Integer
          -> String
          -> C.Range Integer
          -> a
          -> Maybe String
      onS rng nv rng2 _ =
          if rng == rng2
            then Just nv
            else Nothing

  it "single element" $
    let ini = C.Sliced [(r1, "x")] (C.Range 0 1000)
    in  C.alterSlice (onS r1 "y") ini `shouldBeS` [(r1, "y")]
  it "single element, no match" $
    let ini = C.Sliced [(r1, "x")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "y") ini `shouldBeS` [(r1, "x")]
  it "two elements, disjoint, no match" $
    let ini = C.Sliced [(r1, "x"), (r3, "y")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "z") ini `shouldBeS` [(r1, "x"), (r3, "y")]
  it "two elements, disjoint, match 1" $
    let ini = C.Sliced [(r1, "x"), (r3, "y")] (C.Range 0 1000)
    in  C.alterSlice (onS r1 "y") ini `shouldBeS` [(r1, "y"), (r3, "y")]
  it "two elements, disjoint, match 2" $
    let ini = C.Sliced [(r1, "x"), (r3, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "y") ini `shouldBeS` [(r1, "x"), (r3, "y")]
  it "two elements, adjacent, match 1, distinct" $
    let ini = C.Sliced [(r1, "x"), (r2, "y")] (C.Range 0 1000)
    in  C.alterSlice (onS r1 "a") ini `shouldBeS` [(r1, "a"), (r2, "y")]
  it "two elements, adjacent, match 2, distinct" $
    let ini = C.Sliced [(r1, "x"), (r2, "y")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "a") ini `shouldBeS` [(r1, "x"), (r2, "a")]
  it "two elements, adjacent, match 1, joining" $
    let ini = C.Sliced [(r1, "x"), (r2, "y")] (C.Range 0 1000)
    in  C.alterSlice (onS r1 "y") ini `shouldBeS` [(C.Range 100 199, "y")]
  it "two elements, adjacent, match 2, joining" $
    let ini = C.Sliced [(r1, "x"), (r2, "y")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "x") ini `shouldBeS` [(C.Range 100 199, "x")]
  it "three elements, disjoint, match 1" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r1 "a") ini `shouldBeS` [(r1, "a"), (r3, "y"), (r5, "z")]
  it "three elements, disjoint, match 2" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "a") ini `shouldBeS` [(r1, "x"), (r3, "a"), (r5, "z")]
  it "three elements, disjoint, match 3" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r5 "a") ini `shouldBeS` [(r1, "x"), (r3, "y"), (r5, "a")]
  it "three elements, joined l, match 1, distinct" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "a") ini `shouldBeS` [(r2, "a"), (r3, "y"), (r5, "z")]
  it "three elements, joined l, match 2, distinct" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "a") ini `shouldBeS` [(r2, "x"), (r3, "a"), (r5, "z")]
  it "three elements, joined l, match 1, merging" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "y") ini `shouldBeS` [(C.Range 150 249, "y"), (r5, "z")]
  it "three elements, joined l, match 2, merging" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r5, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "x") ini `shouldBeS` [(C.Range 150 249, "x"), (r5, "z")]
  it "three elements, joined r, match 1, distinct" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r1 "a") ini `shouldBeS` [(r1, "a"), (r3, "y"), (r4, "z")]
  it "three elements, joined r, match 2, distinct" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "a") ini `shouldBeS` [(r1, "x"), (r3, "a"), (r4, "z")]
  it "three elements, joined r, match 3, distinct" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r4 "a") ini `shouldBeS` [(r1, "x"), (r3, "y"), (r4, "a")]
  it "three elements, joined r, match 2, merging" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "z") ini `shouldBeS` [(r1, "x"), (C.Range 200 299, "z")]
  it "three elements, joined r, match 3, merging" $
    let ini = C.Sliced [(r1, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r4 "y") ini `shouldBeS` [(r1, "x"), (C.Range 200 299, "y")]
  it "three elements, all joined, match 1, distinct" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "a") ini `shouldBeS` [(r2, "a"), (r3, "y"), (r4, "z")]
  it "three elements, all joined, match 2, distinct" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "a") ini `shouldBeS` [(r2, "x"), (r3, "a"), (r4, "z")]
  it "three elements, all joined, match 3, distinct" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r4 "a") ini `shouldBeS` [(r2, "x"), (r3, "y"), (r4, "a")]
  it "three elements, all joined, match 1, merging" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r2 "y") ini `shouldBeS` [(C.Range 150 249, "y"), (r4, "z")]
  it "three elements, all joined, match 3, merging" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r4 "y") ini `shouldBeS` [(r2, "x"), (C.Range 200 299, "y")]
  it "three elements, all joined, match 2, merging l" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "x") ini `shouldBeS` [(C.Range 150 249, "x"), (r4, "z")]
  it "three elements, all joined, match 2, merging r" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "z")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "z") ini `shouldBeS` [(r2, "x"), (C.Range 200 299, "z")]
  it "three elements, all joined, match 2, merging all" $
    let ini = C.Sliced [(r2, "x"), (r3, "y"), (r4, "x")] (C.Range 0 1000)
    in  C.alterSlice (onS r3 "x") ini `shouldBeS` [(C.Range 150 299, "x")]

main :: IO ()
main = hspec $ do
  it "rangesN" $ property $ forAll (rangesN 10) wellFormed
  it "wellFormedRange" $ property $ \s e -> C.wellFormedRange (C.Range (s :: Integer) e) == (s <= e)
  describe "insertSlice" insertSliceTests
  describe "allocateSlice" allocateSlice
  describe "alterSlice" alterSlice
