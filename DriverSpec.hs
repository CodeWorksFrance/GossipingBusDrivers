import Test.Hspec
import qualified Data.Map as M

type Routes = [Route]
type Route = [Station]
type Station = Int 
type Solution = Int
type Driver = Int
type Knowledge = M.Map Driver [Driver]

main = hspec $ do
    describe "The number of stops it takes for all drivers to be up to date" $
        it "should no stops at all when only one driver is present" $
            gossip [[1]] `shouldBe` 0
    describe "The initial state of knowledge of drivers" $ do
        it "should assign, to a lone driver, only their own gossip" $ do
            let firstDriver = 0
            initial [[1]] `shouldBe` M.fromList [(firstDriver, [firstDriver])]                
        it "should assign, to each driver, their own gossip " $ do
            let irrelevant = []
            initial [irrelevant,irrelevant] `shouldBe` M.fromList [(driver, [driver]) | driver <- [0..1]]
    describe "A state of knowledge of drivers" $ do
        describe "should be complete when all drivers share all gossips" $ do
            it "which in the case of one driver is right away" $ do
                complete (initial [[1]]) `shouldBe` True
            it "which is not the case for two drivers initially" $ do
                let irrelevant = []
                complete (initial [irrelevant,irrelevant]) `shouldBe` False
            it "which can be the case for two drivers" $ do
                let [firstDriver, secondDriver] = [0,1]
                    allTwoGossips = [firstDriver, secondDriver]
                complete (M.fromList [(firstDriver,allTwoGossips),(secondDriver,allTwoGossips)]) `shouldBe` True

gossip :: Routes -> Solution
gossip routes | complete (initial routes) = 0
              | otherwise = undefined

initial :: Routes -> Knowledge
initial routes = M.fromList [(driver, [driver]) | driver <- [0..length routes-1]]

complete :: Knowledge -> Bool
complete knowledge = knowledge == initial [[1]]

-- Where are we going ?
-- - use complete to implement gossip
-- - evolve the state of knowledge
-- - remove the fake from complete
-- - zip cycle the routes