module Solution475Spec where

import Test.Hspec
import Solution475

spec :: Spec
spec = do
    describe "Solution 475" $ do
        it "findHeater [1,2,3] 2 = 1" $ do
            findHeater 2 [1,2,3] `shouldBe` 1
        it "findHeater [1,2,3,6,7] 6 = 3" $ do
            findHeater 6 [1,2,3,6,7] `shouldBe` 3
        it "findRadius [1,2,3], [2] = 1" $ do
            let houses = [1, 2, 3]
            let heaters = [2]
            findRadius houses heaters `shouldBe` 1
        it "findRadius [1,3,2], [2] = 1" $ do
            let houses = [1, 3, 2]
            let heaters = [2]
            findRadius houses heaters `shouldBe` 1
        it "findRadius [1,3,2,4], [1,4] = 1" $ do
            let houses = [1, 2, 3, 4]
            let heaters = [1, 4]
            findRadius houses heaters `shouldBe` 1
        it "findRadius [1,5], [2] = 3" $ do
            let houses = [1, 5]
            let heaters = [2]
            findRadius houses heaters `shouldBe` 3
        it "findRadius [1,5], [10] = 9" $ do
            let houses = [1, 5]
            let heaters = [10]
            findRadius houses heaters `shouldBe` 9
        it "findRadius unsorted heaters" $ do
            let houses = [282475249, 622650073, 984943658, 144108930, 470211272, 101027544, 457850878, 458777923]
            let heaters = [823564440, 115438165, 784484492, 74243042, 114807987, 137522503, 441282327, 16531729, 823378840, 143542612]
            findRadius houses heaters `shouldBe` 161834419
        it "findRadius [474833169,264817709,998097157,817129560],[197493099,404280278,893351816,505795335]" $ do
            let houses = [474833169, 264817709, 998097157, 817129560]
            let heaters = [197493099, 404280278, 893351816, 505795335]
            findRadius houses heaters `shouldBe` 104745341            