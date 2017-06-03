import Data.Maybe
import Data.Monoid
import Test.Hspec

import Cyrats

main :: IO ()
main = hspec $ ratHullSpec

ratHullSpec = do
    describe "Rat.animate" $ do
        it "makes no rats if the hull is incomplete" $
            animate (Hull (m 1 2 3) Nothing (m 4 5 6)) `shouldSatisfy` isNothing
        it "makes a normal rat otherwise" $
            animate (Hull (m 1 2 3) (m 10 20 30) (m 4 5 6)) `shouldSatisfy`
            (\(Just (Rat _ h a d)) -> (h, a, d) == (15, 27, 39))
    describe "Rat.isEnoughFor" $ do
        let someHull = Hull (m 1 1 1) (m 1 2 1) (m 1 1 1)
        it "returns True if energy is enough" $
            someHull `shouldSatisfy` isEnoughFor 10
        it "returns False otherwise" $
            someHull `shouldSatisfy` (not . isEnoughFor 9)
  where
    m x y z = Just $ Module (x, y, z)
