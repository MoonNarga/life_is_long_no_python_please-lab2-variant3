import BST
  ( BST (Empty, Node),
    fromList,
    lchild,
    rchild,
    singleton,
    stateConcat,
    stateFilter,
    stateInsert,
    stateIsEmpty,
    stateMap,
    stateMember,
    stateReduce,
    stateRemove,
    stateSize,
    stateToList,
    value,
  )
import Control.Monad.State
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig specs

specs :: Spec
specs = do
  let int4 = 4 :: Int
  let noInts = [] :: [Int]

  it "data is retained" $
    value (singleton int4) `shouldBe` 4

  it "inserting less" $ do
    let t = execState (stateInsert 2) (singleton int4)
    value t `shouldBe` 4
    value (lchild t) `shouldBe` 2

  it "inserting same" $ do
    let t = execState (stateInsert 4) (singleton int4)
    value t `shouldBe` 4
    lchild t `shouldBe` Empty
    rchild t `shouldBe` Empty

  it "inserting right" $ do
    let t = execState (stateInsert 5) (singleton int4)
    value t `shouldBe` 4
    value (rchild t) `shouldBe` 5

  it "Empty list to tree" $
    fromList noInts `shouldBe` Empty

  it "inserting into Empty" $ do
    let t = execState (stateInsert int4) Empty
    value t `shouldBe` 4

  it "complex tree" $ do
    let t = fromList [int4, 2, 6, 1, 3, 7, 5]
    evalState (stateMember 3) t `shouldBe` True
    evalState (stateMember 9) t `shouldBe` False
    value t `shouldBe` 4
    value (lchild t) `shouldBe` 2
    value (lchild (lchild t)) `shouldBe` 1
    value (rchild (lchild t)) `shouldBe` 3
    value (rchild t) `shouldBe` 6
    value (lchild (rchild t)) `shouldBe` 5
    evalState (stateFilter even) t `shouldBe` [2, 4, 6]
    evalState stateToList (execState (stateMap (* 2)) t) `shouldBe` [2, 4, 6, 8, 10, 12, 14]
    evalState (stateReduce (+) 0) t `shouldBe` 28
    evalState (stateReduce (*) 1) t `shouldBe` 5040
    evalState stateToList (execState (stateRemove 3) t) `shouldBe` [1, 2, 4, 5, 6, 7]

  it "Empty tree to list" $
    length (evalState stateToList Empty) `shouldBe` 0

  it "Iterating over complex tree" $
    evalState stateToList (fromList [int4, 2, 1, 3, 6, 7, 5]) `shouldBe` [1 .. 7]

  it "Test for do" $ do
    let t = fromList [9, 2, 4]
    let t' = fromList [7, 3]
    let t'' = execState (stateConcat t') t
    evalState stateToList t'' `shouldBe` [2, 3, 4, 7, 9]
    evalState stateToList (execState stateDoSome t'') `shouldBe` [2, 3, 4, 9, 10, 13, 20]

stateDoSome :: (Ord a, Num a) => State (BST a) ()
stateDoSome = do
  stateInsert 10
  stateInsert 13
  stateRemove 7
  stateInsert 20
