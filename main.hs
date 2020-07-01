module Main where
import System.Environment

f :: [String] -> [Int]
f x = case x of 
  [] -> [0, 0]
  (a:[]) -> [(read a), 0]
  b -> map read b

main :: IO ()
main = do
  args <- getArgs
  let args1 = f args
  let result = args1 !! 0 + args1 !! 1 
  putStrLn ("Hello, " ++ show result)
