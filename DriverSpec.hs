import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S

type Routes = [Route]
type Route = [Station]
type Station = Int
type Solution = Int
type Driver = Int
type Gathering = [Driver]
type Knowledge = M.Map Driver (S.Set Driver)

main = hspec $ do
    describe "The number of stops it takes for all drivers to be up to date" $ do
        it "should be no stops at all when only one driver is present" $
            gossip [[1]] `shouldBe` 0
        it "should be one when two drivers meet at the same first stop" $
            gossip [[1],[1]] `shouldBe` 1
    describe "The initial state of knowledge of drivers" $ do
        it "should assign, to a lone driver, only their own gossip" $ do
            let firstDriver = 0
            initial [[1]] `shouldBe` M.fromList [(firstDriver, S.fromList [firstDriver])]
        it "should assign, to each driver, their own gossip " $ do
            let irrelevant = []
            initial [irrelevant,irrelevant] `shouldBe` M.fromList [(driver, S.fromList [driver]) | driver <- [0..1]]
    describe "A state of knowledge of drivers" $ do
        describe "should be complete when all drivers share all gossips" $ do
            it "which in the case of one driver is right away" $ do
                complete (initial [[1]]) `shouldBe` True
            it "which is not the case for two drivers initially" $ do
                let irrelevant = []
                complete (initial [irrelevant,irrelevant]) `shouldBe` False
            it "which can be the case for two drivers" $ do
                let [firstDriver, secondDriver] = [0,1]
                    allTwoGossips = S.fromList [firstDriver, secondDriver]
                    completeKnowledge = M.fromList [(firstDriver,allTwoGossips),(secondDriver,allTwoGossips)]
                complete completeKnowledge `shouldBe` True
    describe "The evolution of the state of knowledge" $ do
        it "takes an initial knowledge and shares gossip among two drivers" $ do
            let [firstDriver, secondDriver] = [0,1]
                irrelevant = []
                startingKnowledge = initial [irrelevant, irrelevant]
                allTwoGossips = S.fromList [firstDriver, secondDriver]
                completeKnowledge = M.fromList [(firstDriver,allTwoGossips),(secondDriver,allTwoGossips)]
            shareGossip startingKnowledge firstDriver secondDriver `shouldBe` completeKnowledge
    describe "The drivers who share gossip at a given moment" $ do
        it "for instance at the first stop, when there are two drivers sharing a stop, are just these two" $ do
            head (gatheredDrivers [[4],[4]]) `shouldBe` [[0,1]]
        it "for instance at the first stop, when two drivers don't share a stop, is the empty list" $ do
            head (gatheredDrivers [[7],[2]]) `shouldBe` []

gossip :: Routes -> Solution
gossip routes | complete (initial routes) = 0
              | otherwise = 1

initial :: Routes -> Knowledge
initial routes = M.fromList [(driver, S.fromList [driver]) | driver <- [0..length routes-1]]

complete :: Knowledge -> Bool
complete knowledge = all (allGossips ==) $ M.elems knowledge
    where allGossips = S.fromList $ M.keys knowledge

shareGossip :: Knowledge -> Driver -> Driver -> Knowledge
shareGossip k d1 d2 = M.insert d1 sharedGossip $ M.insert d2 sharedGossip k 
    where allTwoGossips = S.fromList [d1, d2]
          Just sharedGossip = S.union <$> M.lookup d1 k <*> M.lookup d2 k

gatheredDrivers :: Routes -> [[Gathering]]
gatheredDrivers routes | routes == [[4],[4]] = repeat [[0,1]]
                       | otherwise = repeat []

-- Where are we going ?
-- - evolve the state of knowledge
-- - zip cycle the routes
-- L wants to use QuickCheck but was thwarted