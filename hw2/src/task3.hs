module Task3 where 

import Control.Applicative
import Data.Char
import Data.List
import Control.Monad

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap g (Parser a) = Parser f where 
    f xs = case a xs of
      Nothing -> Nothing
      Just (c, cs) -> Just (g c, cs)

instance Applicative (Parser s) where
    pure a = Parser (\x -> Just(a, x))
    Parser u <*> Parser v = Parser f where 
        f xs = case u xs of
            Nothing -> Nothing
            Just (g, xss) -> case v xss of 
                Nothing -> Nothing
                Just (x, xsss) -> Just (g x, xsss)

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  Parser u <|> Parser v = Parser f where 
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

instance Monad (Parser s) where
  return x = Parser (\cs -> Just (x, cs))
  p >>= f = Parser (\cs -> case runParser p cs of
                       Nothing -> Nothing
                       Just (x, xs) -> runParser (f x) xs)

ok :: Parser tok ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser tok ()
eof = Parser $ \s -> case s of 
  [] -> Just ((), [])
  _  -> Nothing

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
  f (c : cs) | pr c  = Just (c, cs)
  f _ = Nothing

element :: (Eq tok) => tok -> Parser tok tok
element c = satisfy (== c)

stream :: (Eq tok) => [tok] -> Parser tok [tok]
stream = foldr (\c -> (<*>) ((:) <$> element c)) (return [])

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit
 
fromDigits :: [Int] -> Int
fromDigits xs = aux xs 0
    where aux xss acc = foldl (\ ac x -> (ac * 10) + x) acc xss

nat :: Parser Char Int
nat = (fromDigits <$> some digit) <|> 
  (element '+' *>  (fromDigits <$> some digit)) <|> 
  (element '-' *>  ((* (-1)) . fromDigits <$> some digit))

addBrackets :: String -> String
addBrackets s = "(" ++ s ++ ")"

brackets :: Parser Char String
brackets = intercalate "" <$> some (element '(' *> (addBrackets . intercalate "" <$> many brackets) <*  element ')')

cnt :: Int -> Parser b a -> Parser b [a]
cnt n v = many_v n where
  many_v i = some_v i <|> pure []
  some_v i = if i > 1 then liftA2 (:) v $ many_v (i - 1)
             else pure []

helper :: Int -> Parser l a -> Parser l b -> Parser l [a]
helper n p1 p2 = fmap (:) p1 <*> cnt n (fmap (const id) p2 <*> p1)

spaces :: Parser Char ()
spaces = void $ many $ satisfy (== ' ')

listN :: Int -> Parser Char [Int]
listN n = helper n nat $ fmap (\_ y _ -> y) spaces <*> element ',' <*> spaces

list :: Parser Char [Int]
list = Parser $ runParser (spaces *> nat) >=>
  (\(len, t) -> if len > 0
    then runParser (spaces *> element ',' *> spaces *> listN len) t
    else Just ([], t))

lists ::  Parser Char [[Int]]
lists = some ((list <* spaces <* element ',' <* spaces) <|> list) 












