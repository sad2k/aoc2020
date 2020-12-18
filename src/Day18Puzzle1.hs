import System.IO
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

data Operator = Add | Multiply deriving (Eq, Ord, Show)

data Expr = Value Integer | ApplyOp Operator Expr Expr deriving Show

parseNumber :: Parser Expr
parseNumber = do
    n <- many $ oneOf "0123456789"
    return $ Value (read n)

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') p

parseExpr :: Parser Expr
parseExpr = chainl1 parseTerm parseOperation

parseTerm :: Parser Expr
parseTerm = (parens parseExpr) <|> parseNumber

parseOperation :: Parser (Expr -> Expr -> Expr)
parseOperation = do
    ch <- oneOf "+*"
    case ch of
        '+' -> return $ ApplyOp Add
        '*' -> return $ ApplyOp Multiply

eval :: Expr -> Integer
eval (Value x) = x
eval (ApplyOp Add v1 v2) = eval v1 + eval v2
eval (ApplyOp Multiply v1 v2) = eval v1 * eval v2

parseLine :: String -> Integer
parseLine expr = case (parse parseExpr "" filteredExpr) of
                  Left err -> error "error parsing"
                  Right e -> eval e
    where filteredExpr = filter (/=' ') expr

run h = do
    contents <- hGetContents h
    let ls = lines contents
    print $ sum $ map parseLine ls

main = withFile "input/Day18.txt" ReadMode run


