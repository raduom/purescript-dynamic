module Test.Main where

import Control.Monad.Eff (Eff)
import Data.Dynamic (fromDynamic, toDynamic)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Data.Typeable (class Typeable, class Typeable1, TyCon(..), TypeRep(..))
import Prelude (class Eq, class Show, Unit, show, (<>), (==), bind)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

data LV a = LV Int
          | LA a

instance typeable1LV :: Typeable1 LV where
  typeOf1 _ = TypeRep (TyCon { tyConModule: "Test.Main", tyConName: "LV" }) Nil

instance eqLV :: (Eq a) => Eq (LV a) where
  eq (LV u) (LV v) = u == v
  eq (LA u) (LA v) = u == v
  eq _ _ = false

instance showLV :: (Show a) => Show (LV a) where
  show (LV u) = "LV " <> show u
  show (LA u) = "LA " <> show u

data T = A String
       | B Int

newtype C = C Int

instance someTypeable :: Typeable T where
  typeOf _ = TypeRep (TyCon { tyConModule: "Test.Main", tyConName: "T" }) Nil

instance someShow :: Show T where
  show (A s) = "A " <> show s
  show (B n) = "B " <> show n

instance someEq :: Eq T where
  eq (A u) (A v) = u == v
  eq (B u) (B v) = u == v
  eq _ _ = false

instance intTypeable :: Typeable C where
  typeOf _ = TypeRep (TyCon { tyConModule: "Test.Main", tyConName: "C" }) Nil

instance intShow :: Show C where
  show (C n) = "C " <> show n

instance intEq :: Eq C where
  eq (C u) (C v) = u == v

someA :: T
someA = A "Some"

someB :: T
someB = B 5

someC :: C
someC = C 42

someLV :: LV String
someLV = LA "something"


main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Dynamic" do
    describe "Converting some typeable data to dynamic and back" do
      it "should return the original for the first branch" do
        fromDynamic (toDynamic someA) `shouldEqual` Just someA
      it "should return the original for the second branch" do
        fromDynamic (toDynamic someB) `shouldEqual` Just someB
    describe "Converting a typeable to the wrong type" do
      it "should return Nothing for any branch" do
        let dyn = toDynamic someC
        let cast = (fromDynamic dyn) :: Maybe T
        cast `shouldEqual` Nothing
    describe "Converting a Typeable1 to dyanmic and back" do
      it "should return the original" do
        fromDynamic (toDynamic someLV) `shouldEqual` Just someLV
    describe "Converting a typeable to the wrong type" do
      it "should return Nothing for any branch" do
        let dyn = toDynamic someC
        let cast = (fromDynamic dyn) :: Maybe (LV String)
        cast `shouldEqual` Nothing
