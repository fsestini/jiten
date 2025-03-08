module Main (main) where

import qualified Jiten.DatabaseSpec
import qualified Jiten.YomichanSpec
import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Jiten.Database" Jiten.DatabaseSpec.spec
  describe "Jiten.Yomichan" Jiten.YomichanSpec.spec
