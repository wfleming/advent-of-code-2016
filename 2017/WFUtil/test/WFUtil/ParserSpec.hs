module WFUtil.ParserSpec where

import Test.Hspec
import Test.QuickCheck

import WFUtil.Parser as P

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "words" $ do
    it "parses whitespace separated words" $ do
      let r = parseStr P.words "foo bar"
      r `shouldBe` Right ["foo", "bar"]

    it "parses variable whitespace" $ do
      let r = parseStr P.words "foo   bar\t buz"
      r `shouldBe` Right ["foo", "bar", "buz"]

  -- describe "lineSeparated" $ do
  --   it "processes other parser by lines" $ do
  --     let r = parseStr (P.lineSeparated P.words) "foo  bar\nbuz\tboo"
  --     r `shouldBe` Right [["foo", "bar"], ["buz", "boo"]]
