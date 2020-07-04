module Do_liftM_bind_example where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Number Integer deriving Show
  
f :: [Char]
f = do
  x <- "0123456"
  guard (x /= '0')
  return x

parseNumber :: Parser LispVal
parseNumber = do
  x <- many1 digit
  return $ Number (read x)

parseNumber' :: Parser LispVal
parseNumber' = liftM (Number . read) $ many1 digit

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number . read


readExpr :: String -> String
readExpr input = case parse parseNumber'' "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value" ++ show val



-- usage main 
--       "param"
main :: IO ()
main = do
    expr <- readLn
    putStrLn (readExpr expr)
