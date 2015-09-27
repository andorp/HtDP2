module Test.Check where

checkExpect :: (Eq a) => String -> a -> a -> IO ()
checkExpect name expect actual =
  putStrLn $ name ++ ": " ++ if expect == actual
                                then "Passed"
                                else "Failed"
