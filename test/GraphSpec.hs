module GraphSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Data.List
import Graph


smallGraphs :: Int -> Gen (Blind (Graph Int))
smallGraphs 0 = return $ Blind $ makeGraph []
smallGraphs n = (Blind . makeGraph . foldr add [(i,[]) | i <- [1..n]])
           <$> (resize (round $ (fromIntegral n)**1.7) $ listOf edge)
           where
              edge :: Gen (Int, Int)
              edge = do
                a <- choose (1,n)
                b <- choose (1,a)
                return (b,a)
              add :: (Int, Int) -> [(Int, [Int])] -> [(Int, [Int])]
              add (a,b) xs = take (a-1) xs
                          ++ [(a, if b `elem` as then as else b:as)]
                          ++ (drop a $ take (b-1) xs ++ [(b, if a `elem` bs then bs else a:bs)])
                          ++ drop b xs
                          where
                            (_, as) = xs !! (a-1)
                            (_, bs) = xs !! (b-1)

graphGen :: Gen (Blind (Graph Int))
graphGen = sized smallGraphs

simpleGraph :: Graph Int
simpleGraph = makeGraph [(0, [1,2]), (1, [0,2]), (2, [0,1])]

simpleRing :: Graph Char
simpleRing = makeGraph [('a', ['b', 'e']), ('b', ['a','c']), ('c', ['b','d']), ('d', ['c', 'e']), ('e', ['a', 'd'])]

moreComplexGraph :: Graph Int
moreComplexGraph = makeGraph [(0, [1, 2, 5]), (1, [0,7]), (2, [0,6]), (3, [5]), (4, [5]), (5, [0,3,4]), (6, [2, 6]), (7, [1]), (8, [9]), (9, [8]), (10, [])]

spec :: Spec
spec = do
  describe "isNeighbour" $ do
    context "when using ring of 3 ints" $ do
      it "correctly identifies neighbours" $ do
        isNeighbour simpleGraph 0 1 `shouldBe` True
      it "correctly identifies neighbours (reversed)" $ do
        isNeighbour simpleGraph 2 1 `shouldBe` True
      it "correctly identifies (nonconsecutive) neighbours" $ do
        isNeighbour simpleGraph 2 0 `shouldBe` True
      it "correctly identifies non-neighbours (when node isn't connected to itself)" $ do
        isNeighbour simpleGraph 1 1 `shouldBe` False
    context "when using ring of 5 chars" $ do
      it "correctly identifies neighbours" $ do
        isNeighbour simpleRing 'c' 'd' `shouldBe` True
      it "correctly identifies neighbours (reversed)" $ do
        isNeighbour simpleRing 'b' 'a' `shouldBe` True
      it "correctly identifies (nonconsecutive) neighbours" $ do
        isNeighbour simpleRing 'a' 'e' `shouldBe` True
      it "correctly identifies nonconsecutive non-neighbours" $ do
        isNeighbour simpleRing 'b' 'e' `shouldBe` False
      it "correctly identifies non-neighbours (when node isn't connected to itself)" $ do
        isNeighbour simpleRing 'c' 'c' `shouldBe` False
    context "when using (non connected) graph of 11 integers" $ do
      it "correctly identifies neighbours" $ do
        isNeighbour moreComplexGraph 5 4 `shouldBe` True
      it "correctly identifies self-neighbor" $ do
        isNeighbour moreComplexGraph 6 6 `shouldBe` True
      it "correctly identifies non-neighbours" $ do
        isNeighbour moreComplexGraph 1 6 `shouldBe` False
      it "correctly identifies non-neighbours (in different components)" $ do
        isNeighbour moreComplexGraph 7 6 `shouldBe` False
      it "correctly identifies non-neighbor for singleton" $ do
        isNeighbour moreComplexGraph 10 10 `shouldBe` False
    context "for random small graphs" $ do
      it "is symmertic (so isNeighbour g a b => isNeighbour g b a)" $ property $
          forAll (elements [2..10])
          $ \n -> forAll (smallGraphs n)
          $ \g -> forAll (elements [1..n])
          $ \a -> forAll (elements [1..n])
          $ \b -> isNeighbour (getBlind g) a b ==> isNeighbour (getBlind g) b a
  describe "neighbours" $ do
    context "when using ring of 3 ints" $ do
      it "correctly identifies neighbours of 0" $ do
        sort (neighbours simpleGraph 0) `shouldBe` [1,2]
      it "correctly identifies neighbours of 2" $ do
        sort (neighbours simpleGraph 2) `shouldBe` [0,1]
    context "when using ring of 5 chars" $ do
      it "correctly identifies neighbours of 'a'" $ do
        sort (neighbours simpleRing 'a') `shouldBe` ['b','e']
      it "correctly identifies neighbours of 'c'" $ do
        sort (neighbours simpleRing 'c') `shouldBe` ['b','d']
    context "when using (non connected) graph of 11 integers" $ do
      it "correctly identifies neighbours of 0" $ do
        sort (neighbours moreComplexGraph 0) `shouldBe` [1,2,5]
      it "correctly identifies neighbours of 5" $ do
        sort (neighbours moreComplexGraph 5) `shouldBe` [0,3,4]
      it "correctly identifies neighbours of 6" $ do
        sort (neighbours moreComplexGraph 6) `shouldBe` [2,6]
      it "correctly identifies neighbours of 10" $ do
        sort (neighbours moreComplexGraph 10) `shouldBe` []
    context "for random small graphs" $ do
      it "ensures all neighbours (are) isNeighbour" $ property $
          forAll (elements [2..10])
          $ \n -> forAll (smallGraphs n)
          $ \g -> forAll (elements [1..n])
          $ \a -> (not $ null (neighbours (getBlind g) a))
          ==> forAll (elements (neighbours (getBlind g) a))
          $ \b -> isNeighbour (getBlind g) a b
      it "ensures all neighbours in both lists" $ property $
          forAll (elements [2..10])
          $ \n -> forAll (smallGraphs n)
          $ \g -> forAll (elements [1..n])
          $ \a -> (not $ null (neighbours (getBlind g) a))
          ==> forAll (elements (neighbours (getBlind g) a))
          $ \b -> a `elem` neighbours (getBlind g) b
      