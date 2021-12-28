{-# LANGUAGE OverloadedStrings #-}
module Parser where 

import Math
import Program

import Control.Applicative
import Control.Monad.Catch 
import Control.Monad.State
import Data.Typeable            
import Data.Void   
import Text.Megaparsec
import Text.Megaparsec.Byte                                          
import Text.Megaparsec.Expr
import qualified Data.Map.Strict as M             
import qualified Data.ByteString 
import qualified Data.ByteString.Internal
import qualified Data.ByteString.UTF8 as S
import qualified Text.Megaparsec.Byte.Lexer as L
import qualified Text.Megaparsec.Char.Lexer 

type Str = S.ByteString

type Parser = Parsec Void Str

newtype ParsingException = ParsingException (ParseError (Token Str) Void)
  deriving (Typeable)

instance Show ParsingException where
    show (ParsingException e) = parseErrorPretty e

instance Exception ParsingException

indent :: Parser ()
indent = skipMany (char (Data.ByteString.Internal.c2w ' ') <|> char (Data.ByteString.Internal.c2w '\t'))

sc :: Parser ()
sc = L.space Text.Megaparsec.Byte.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Str -> Parser Str
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rword :: Str -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: Parser Str
identifier = (lexeme . Text.Megaparsec.try) (p >>= check)
  where
    p = Data.ByteString.pack <$> ((:) <$> letterChar <*> Text.Megaparsec.many alphaNumChar)
    check x = if x `elem` rws
      then fail $ "keyword " ++ show x ++ " cannot be an identifier"
      else return x
    rws :: [Str] 
    rws = ["let", "mut", "for", "break"]

term :: Parser Expr
term = parens expr
  <|> (Let <$> (rword "let" *> (S.toString <$> identifier) <* symbol "=") <*> (expr <* symbol "in" ) <*> expr)
  <|> Var <$> (S.toString <$> identifier)
  <|> Lit <$> integer

expr :: Parser Expr
expr = makeExprParser term operators
  where
    operators :: [[Operator Parser Expr]]
    operators = [[InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/")], [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]]

newVarStmt :: Parser Statement
newVarStmt = NewVarStat <$> (rword "mut" *> (S.toString <$> identifier) <* symbol "=") <*> expr

printStatStmt :: Parser Statement
printStatStmt = PrintStat <$> (symbol "<" *> expr)

readStatStmt :: Parser Statement
readStatStmt = ReadStat <$> (symbol ">" *> (S.toString <$> identifier))

updVarStatStmt :: Parser Statement
updVarStatStmt = UpdVarStat <$> ((S.toString <$> identifier) <* symbol "=") <*> expr

forStatStmt :: Parser Statement
forStatStmt = do
  _ <- rword "for" *> symbol "("
  ini <- expr
  _ <- rword "to"
  bnd <- expr
  _ <- symbol ")" *> symbol "{" *> sc
  inner <- programParser
  _ <- sc
  _ <- symbol "}"
  return $ ForStat ini bnd inner 

breakStatStmt :: Parser Statement
breakStatStmt = do 
  _ <- rword "break" *> sc
  return BreakStat

stmt :: Parser Statement
stmt = indent *> (newVarStmt <|> printStatStmt <|> readStatStmt <|> updVarStatStmt <|> forStatStmt <|> breakStatStmt)

programParser :: Parser [Statement]
programParser = sepEndBy (Text.Megaparsec.Char.Lexer.nonIndented sc stmt) sc

useParser :: (MonadThrow m) => Parser a -> Str -> m a
useParser p input = either (throwM . ParsingException) return (parse p "" input)

parseAndEval :: (MonadThrow m) => Str -> m Int
parseAndEval input = useParser expr input >>= flip doEval M.empty

parseAndCompute :: (MonadIO m, MonadCatch m) => Str -> m Dictionary
parseAndCompute input = useParser stmt input >>= \x -> fst <$> runStateT (exec [x]) M.empty

runProgram :: Str -> IO ()
runProgram input = useParser programParser input >>= interpret