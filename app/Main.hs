import Text.Parsec
import Text.Parsec.String
import Text.ParserCombinators.Parsec.Number

type Var = String
type Stuff = String
data Statement = Lekha Stuff | Huncha Var Stuff | Jaba Statement Statement Code | Kaam Code | Joda Stuff Stuff deriving (Eq, Show)
type Code = [Statement]

main :: IO ()
main = do
    fileContents <- readFile "nepali"
    let code = parse codeParser "nepali" fileContents
    print code

-- Parser -> String -> Code
-- The parser that takes in the file contents then passes it to the statement parser
codeParser :: Parser Code
codeParser = spaces >> many (statementParser <* many1 space) <* eof

-- the parser that parses everything
statementParser :: Parser Statement
statementParser = try assignParser <|> try printParser <|> try addParser <|> forParser


-- Lekha means "to write" in Nepali, hence it is used here as a print function.
printParser :: Parser Statement
printParser = Lekha <$> (string "lekha" >> many1 space >> many1 alphaNum)

-- Huncha means "is" or "to be" in Nepali, hence it is used here as an assign function.
assignParser :: Parser Statement
assignParser = Huncha <$> many1 letter <*> (many1 space >> string "huncha" >> many1 space >> many1 alphaNum)


-- Joda means "add" in Nepali, as in "Add 1 and 3" and so on.
addParser :: Parser Statement
addParser = Joda <$> many1 alphaNum <*> (many1 space >> string "joda" >> many1 space >> many1 alphaNum)

forParser :: Parser Statement
forParser = Jaba <$> modStatement <*> endPoint <*> instructions
            where
                modStatement = string "jaba" >> many1 space >> statementParser
                endPoint     = many1 space >> string "gardai" >> many1 space >> statementParser
                instructions = many1 space >> string "taba" >> many1 space >> many (statementParser <* many1 space) <* string "bhayo"

-- Code gen -> Code -> String (output.c) 
--TODO
