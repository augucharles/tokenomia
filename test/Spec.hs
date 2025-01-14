{-# LANGUAGE OverloadedStrings #-}
module Spec(main) where

-- import qualified Spec.Tokenomia.Token.CLAPStyle.MonetaryPolicy 
-- import qualified Spec.Tokenomia.Vesting.Contract 
import qualified Spec.Tokenomia.Adapter.Cardano.CLI.UTxO
import qualified Spec.Tokenomia.Adapter.Cardano.CLI.Value
import           Test.Tasty
import           Test.Tasty.Hedgehog       (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "use cases" [
    -- Spec.Tokenomia.Token.CLAPStyle.MonetaryPolicy.tests,
    -- Spec.Tokenomia.Vesting.Contract.tests,
    Spec.Tokenomia.Adapter.Cardano.CLI.UTxO.tests ,
    Spec.Tokenomia.Adapter.Cardano.CLI.Value.tests
    ]
