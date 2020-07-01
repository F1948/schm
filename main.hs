module Main where
import System.Environment

f :: String -> String
f x = case x of 
  "" -> "John"
  b  -> b

main :: IO ()
main = do
  name <- getLine
  let result = f name
  putStrLn ("Hello, " ++ result)
