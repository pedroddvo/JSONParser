module Main where

import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Maybe
import Control.Applicative
import GHC.Unicode (isDigit, isSpace)
import System.IO
import System.Environment

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Integer
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (Map String JsonValue)
  deriving (Show, Eq)

-- Support proper errors
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \src -> do
        (src', x) <- p src
        Just (src', f x)

instance Applicative Parser where
  pure f = Parser $ \src -> Just (src, f)
  Parser p1 <*> Parser p2 = Parser $ \src -> do
    (src', f) <- p1 src
    (src', a) <- p2 src'
    Just (src', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ \src -> p1 src <|> p2 src

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs)
      | x == c = Just (xs, x)
      | otherwise = Nothing
    f [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP f = Parser $ \src -> Just (invert $ span f src)
  where invert (a, b) = (b, a)

notNullP :: Parser [a] -> Parser [a]
notNullP (Parser p) = Parser $ \src -> do
  (src', o) <- p src
  if null o then Nothing else Just (src', o)

wsP :: Parser String
wsP = takeWhileP isSpace

charWsP :: Char -> Parser Char
charWsP c = charP c <* wsP

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP p1 p2 = (append <$> p2 <*> many (p1 *> p2) <* optional p1) <|> pure []
  where append a b = a:b

jsonBool :: Parser JsonValue
jsonBool = JsonBool <$>
  ((True <$ stringP "true") <|>
   (False <$ stringP "false"))

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> notNullP (takeWhileP isDigit)

jsonStringP :: Parser String
jsonStringP = charP '"' *> takeWhileP (/= '"') <* charP '"'

-- Support escapes
jsonString :: Parser JsonValue
jsonString = JsonString <$> jsonStringP

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> p
  where p = charWsP '[' *> elements <* charP ']'
        elements = sepByP (charWsP ',') (jsonValue <* wsP)

jsonObject :: Parser JsonValue
jsonObject = p
  where p  = JsonObject . Map.fromList <$> surrounding
        surrounding = charWsP '{' *> elements <* charP '}'
        elements = sepByP (charWsP ',') (element <* wsP)
        element = (,) <$> (jsonStringP <* wsP) <*> (charWsP ':' *> jsonValue)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = do
  args <- getArgs
  mapM_ parseFile args

parseFile :: String -> IO ()
parseFile file = do
  contents <- readFile file
  let parsed = runParser (jsonValue) contents
  putStrLn $ show parsed
