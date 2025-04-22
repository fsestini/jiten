module Jiten.YomichanSpec where

import qualified Jiten.Yomichan.SearchSpec
import Test.Hspec (Spec, describe)

spec :: Spec
spec = do
  describe "Jiten.Yomichan.SearchSpec" Jiten.Yomichan.SearchSpec.spec
