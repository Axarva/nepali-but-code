import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

type Var = String
type Stuff = String
data Statement = Lekha Stuff | Huncha Stuff Stuff | Jaba Var Code | Mukhya Code | Bhayo | Kaam Code | Joda Int Int deriving (Eq, Show)
type Code = [Statement]

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- Parser -> String -> Code
printParser :: Parser Statement
printParser = Lekha <$> (string "lekha" >> many1 space >> many1 alphaNum)

statementParser :: Parser Statement
statementParser = Huncha <$> many1 letter <*> (many1 space >> string "huncha" >> many1 space >> many1 alphaNum)

addParser :: Parser Statement
addParser = Joda <$> int <*> (many1 space >> string "joda" >> many1 space >> int)

-- Code gen -> Code -> String (output.c)
