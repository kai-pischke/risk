module GraphSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List
import Graph

simpleGraph :: Graph Int
simpleGraph = makeGraph [(0, [1,2]), (1, [0,2]), (2, [0,1])]

simpleRing :: Graph Char
simpleRing = makeGraph [('a', ['b', 'e']), ('b', ['a','c']), ('c', ['b','d']), ('d', ['c', 'e']), ('e', ['a', 'd'])]

moreComplexGraph :: Graph Int 
moreComplexGraph = makeGraph [(0, [1, 2, 5]), (1, [0,7]), (2, [0,6]), (3, [5]), (4, [5]), (5, [0,3,4]), (6, [2, 6]), (7, [1]), (8, [9]), (9, [8]), (10, [])]

spec :: Spec
spec = do
  describe "isNeighbor" $ do
    context "when using ring of 3 ints" $ do 
      it "correctly identifies neighbors" $ do
        isNeighbor simpleGraph 0 1 `shouldBe` True
      it "correctly identifies neighbors (reversed)" $ do
        isNeighbor simpleGraph 2 1 `shouldBe` True
      it "correctly identifies (nonconsecutive) neighbors" $ do
        isNeighbor simpleGraph 2 0 `shouldBe` True
      it "correctly identifies non-neighbors (when node isn't connected to itself)" $ do
        isNeighbor simpleGraph 1 1 `shouldBe` False
    context "when using ring of 5 chars" $ do 
      it "correctly identifies neighbors" $ do
        isNeighbor simpleRing 'c' 'd' `shouldBe` True
      it "correctly identifies neighbors (reversed)" $ do
        isNeighbor simpleRing 'b' 'a' `shouldBe` True
      it "correctly identifies (nonconsecutive) neighbors" $ do
        isNeighbor simpleRing 'a' 'e' `shouldBe` True
      it "correctly identifies nonconsecutive non-neighbors" $ do
        isNeighbor simpleRing 'b' 'e' `shouldBe` False
      it "correctly identifies non-neighbors (when node isn't connected to itself)" $ do
        isNeighbor simpleRing 'c' 'c' `shouldBe` False
    context "when using (non connected) graph of 11 integers" $ do 
      it "correctly identifies neighbors" $ do 
        isNeighbor moreComplexGraph 5 4 `shouldBe` True
      it "correctly identifies self-neighbor" $ do
        isNeighbor moreComplexGraph 6 6 `shouldBe` True 
      it "correctly identifies non-neighbors" $ do 
        isNeighbor moreComplexGraph 1 6 `shouldBe` False
      it "correctly identifies non-neighbors (in different components)" $ do 
        isNeighbor moreComplexGraph 7 6 `shouldBe` False 
      it "correctly identifies non-neighbor for singleton" $ do 
        isNeighbor moreComplexGraph 10 10 `shouldBe` False    
  describe "neighbors" $ do
    context "when using ring of 3 ints" $ do 
      it "correctly identifies neighbors of 0" $ do
        sort (neighbors simpleGraph 0) `shouldBe` [1,2]
      it "correctly identifies neighbors of 2" $ do
        sort (neighbors simpleGraph 2) `shouldBe` [0,1]
    context "when using ring of 5 chars" $ do 
      it "correctly identifies neighbors of 'a'" $ do
        sort (neighbors simpleRing 'a') `shouldBe` ['b','e']
      it "correctly identifies neighbors of 'c'" $ do
        sort (neighbors simpleRing 'c') `shouldBe` ['b','d']
    context "when using (non connected) graph of 11 integers" $ do 
      it "correctly identifies neighbors of 0" $ do 
        sort (neighbors moreComplexGraph 0) `shouldBe` [1,2,5]
      it "correctly identifies neighbors of 5" $ do
        sort (neighbors moreComplexGraph 5) `shouldBe` [0,3,4] 
      it "correctly identifies neighbors of 6" $ do 
        sort (neighbors moreComplexGraph 6) `shouldBe` [2,6] 
      it "correctly identifies neighbors of 10" $ do 
        sort (neighbors moreComplexGraph 10) `shouldBe` [] 


