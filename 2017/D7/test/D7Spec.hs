module D7Spec where

import Test.Hspec

import Text.Megaparsec (parse, ParseError, Dec)
import D7Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "progParser" $ do
    it "parses program attrs" $ do
      let r = parse progParser "NO_INPUT_FILE" "abc (123)"
      r `shouldBe` Right ("abc", 123)

  describe "progDepParser" $ do
    it "parses program names" $ do
      let r = parse progDepParser "NO_INPUT_FILE" " -> abc, def, ghi"
      r `shouldBe` Right ["abc", "def", "ghi"]

  describe "lineParser" $ do
    it "parses a line with no prog deps" $ do
      let r = parse lineParser "NO_INPUT_FILE" "abc (123)\n"
      r `shouldBe` Right (("abc", 123), [])

    it "parses a line with prog deps" $ do
      let r = parse lineParser "NO_INPUT_FILE" "abc (123) -> xxx, yyy\n"
      r `shouldBe` Right (("abc", 123), ["xxx", "yyy"])

  describe "buildWeights" $ do
    it "replaces dep names with full prog descriptors" $ do
      let xs =
            [ (("abc", 123), [])
            , (("def", 456), ["abc"]) ]
      let xs' = buildWeights xs
      xs' `shouldBe`
            [ (("abc", 123), [])
            , (("def", 456), [("abc", 123)]) ]

  describe "stubTrees" $ do
    it "creates stub trees" $ do
      let xs =
            [ (("abc", 123), [])
            , (("def", 456), [("abc", 123)]) ]
      let xs' = stubTrees xs
      xs' `shouldBe`
            [ Node { root = ("abc", 123), children = [] }
            , Node { root = ("def", 456), children = [Node { root = ("abc", 123), children = [] }] } ]

  describe "rootName" $ do
    it "identifies the root node" $ do
      let r = rootName <$> parse parser "NO_INPUT_FILE" sampleInputStr
      r `shouldBe` Right "tknk"

  describe "mvCandidates" $ do
    it "identifies move candidates" $ do
      let xs =
           [ Node { root = ("abc", 123), children = [] }
           , Node { root = ("def", 456), children = [Node { root = ("abc", 123), children = [] }] } ]
      let (candidates, others) = mvCandidates xs
      candidates `shouldBe` [Node { root = ("abc", 123), children = [] }]
      others `shouldBe` [Node { root = ("def", 456), children = [Node { root = ("abc", 123), children = [] }]}]

    it "identifies move candidates in the sample input" $ do
      let r = parse parser "NO_INPUT_FILE" sampleInputStr
      let r' = either (error . show) id r
      let (candidates, others) = (mvCandidates . stubTrees . buildWeights) r'
      length candidates `shouldBe` 9
      length others `shouldBe` 4

  describe "buildTree" $ do
    it "identifies the root node" $ do
      let r = (fst . root . buildTree) <$> parse parser "NO_INPUT_FILE" sampleInputStr
      r `shouldBe` Right "tknk"

sampleInputStr :: String
sampleInputStr  =
  "pbga (66)\n" ++
  "xhth (57)\n" ++
  "ebii (61)\n" ++
  "havc (66)\n" ++
  "ktlj (57)\n" ++
  "fwft (72) -> ktlj, cntj, xhth\n" ++
  "qoyq (66)\n" ++
  "padx (45) -> pbga, havc, qoyq\n" ++
  "tknk (41) -> ugml, padx, fwft\n" ++
  "jptl (61)\n" ++
  "ugml (68) -> gyxo, ebii, jptl\n" ++
  "gyxo (61)\n" ++
  "cntj (57)\n"
