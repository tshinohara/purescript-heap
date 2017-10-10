module Test.Main where

import Data.Array (sort)
import Data.Foldable (foldl)
import Data.Heap (Heap, Min, deleteMin, empty, insert, min, null)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Prelude hiding (min)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)


main = run [consoleReporter] do
  describe "Heap" do
    it "insert/min" do
      let h0 = empty :: Heap Min Int
      null h0 `shouldEqual` true
      let h1 = insert 4 h0
      null h1 `shouldEqual` false
      min h1 `shouldEqual` Just 4
      min (deleteMin h1) `shouldEqual` Nothing
    it "sort" do
      let xs = [5,6,1,3,7,2,5,4,2,10,8]
      heapSort xs `shouldEqual` sort xs


heapSort xs =
  foldl (flip insert) empty xs
    # unfoldr \h -> (\m -> Tuple m (deleteMin h)) <$> min h
