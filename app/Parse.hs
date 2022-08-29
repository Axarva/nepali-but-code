  {-# LANGUAGE FlexibleContexts, FlexibleInstances, OverlappingInstances #-}
  module FunSyntax where
  import Text.PrettyPrint.HughesPJ (Doc, (<+>),($$),(<>))
  import qualified Text.PrettyPrint.HughesPJ as PP
  import Parser hiding (get)
  import ParserCombinators
  type Variable = String
  data Bop =
     Plus     -- +  :: Int  -> Int  -> Int
   | Minus    -- -  :: Int  -> Int  -> Int
   | Times    -- (*) :: Int  -> Int  -> Int
   | Gt       -- >  :: Int -> Int -> Bool 
   | Ge       -- >= :: Int -> Int -> Bool
   | Lt       -- <  :: Int -> Int -> Bool
   | Le       -- <= :: Int -> Int -> Bool
      deriving (Eq, Show)
  data Expression =
     Var Variable
   | IntExp  Int
   | BoolExp Bool
   | Op  Bop Expression Expression
   | If Expression Expression Expression
   -- new stuff
   | Fun Variable Expression                -- anonymous function,   'fun x -> e'
   | App Expression Expression              -- function application, 'e1 e2'
   | LetRec Variable Expression Expression  -- (recursive) binding,  'let rec f = e in e'
      deriving (Show, Eq)
  factExp :: Expression
  factExp = LetRec "FACT" (Fun "X" (If
                             (Op Le (Var "X") (IntExp 1)) (IntExp 1)
                               (Op Times (Var "X") (App (Var "FACT") (Op Minus (Var "X") (IntExp 1))))))
           (App (Var "FACT") (IntExp 5))
  varP :: Parser Variable
  varP  = wsP (many1 upper)
  boolP :: Parser Bool
  boolP =  wsP (string "true" >> return True)
       <|> wsP (string "false" >> return False)
  opP :: Parser Bop
  opP =  wsP (string "+"  >> return Plus)
     <|> wsP (string "-"  >> return Minus)
     <|> wsP (string "*"  >> return Times)
     <|> wsP (string ">=" >> return Ge)
     <|> wsP (string "<=" >> return Le)
     <|> wsP (string ">"  >> return Gt)
     <|> wsP (string "<"  >> return Lt)
  parenP :: Parser a -> Parser a
  parenP p = between (wsP (char '(')) p (wsP (char ')'))
  varExprP  = Var     `fmap` wsP varP
  boolExprP = BoolExp `fmap` wsP boolP
  intExprP  = IntExp  `fmap` wsP int
  ifP = do
      wsP $ string "if"
      e1 <- exprP
      wsP $ string "then"
      e2 <- exprP
      wsP $ string "else"
      If e1 e2 <$> exprP
  funP = do
      wsP $ string "fun"
      x <- varP
      wsP $ string "->"
      Fun x <$> exprP
  letrecP = do
      wsP $ string "let rec"
      x <- varP
      wsP $ string "="
      e1 <- exprP
      wsP $ string "in"
      LetRec x e1 <$> exprP
  -- use chainl1 for associativity and precedence
  exprP :: Parser Expression
  exprP = sumP where
    sumP    = prodP   `chainl1` opLevel (level Plus)
    prodP   = compP   `chainl1` opLevel (level Times)
    compP   = appP    `chainl1` opLevel (level Gt)
    appP    = factorP >>= \x ->
                  (many1 factorP >>= \vs -> return (foldl App x vs))
              <|> return x
    factorP = parenP exprP <|> baseP
    baseP   = boolExprP <|> intExprP <|> ifP <|> funP <|> letrecP
           <|> varExprP
  -- only succeeds for operators at a particular precedence level
  opLevel :: Int -> Parser (Expression -> Expression -> Expression)
  opLevel l = do x <- opP
                 if level x == l then return $ Op x else fail ""
  wsP :: Parser a -> Parser a
  wsP p = do x <- p
             many space
             return x
  parse :: String -> Maybe Expression
  parse s = case doParse exprP s of
              [(exp, _)] -> Just exp
              _ -> Nothing
  instance PP Bop where
    pp Plus   =  PP.text "+"
    pp Minus  =  PP.text "-"
    pp Times  =  PP.text "*"
    pp Gt     =  PP.text ">"
    pp Ge     =  PP.text ">="
    pp Lt     =  PP.text "<"
    pp Le     =  PP.text "<="
  class PP a where
    pp :: a -> Doc
  display :: PP a => a -> String
  display = show . pp
  instance PP Variable where
   pp s = PP.text s
  instance PP Expression where
   pp (Var x)  = PP.text x
   pp (IntExp x)   = PP.text (show x)
   pp (BoolExp x)  = if x then PP.text "true" else PP.text "false"
   pp e@Op {} = ppPrec 0 e
   pp (If e s1 s2) =
     PP.vcat [PP.text "if" <+> pp e <+> PP.text "then",
          PP.nest 2 (pp s1),
          PP.text "else",
          PP.nest 2 (pp s2) ]
   pp e@(App _ _) = ppPrec 0 e
   pp (Fun x e)   =
    PP.hang (PP.text "fun" <+> pp x <+> PP.text "->") 2 (pp e)
   pp (LetRec x e1 e2) =
    PP.vcat [PP.text "let rec" <+> pp x <+> PP.text "=",
          PP.nest 2 (pp e1),
          PP.text "in",
          PP.nest 2 (pp e2) ]
  ppPrec n (Op bop e1 e2) =
      parens (level bop < n) $
            ppPrec (level bop) e1 <+> pp bop <+> ppPrec (level bop + 1) e2
  ppPrec n (App e1 e2) =
      parens (levelApp < n) $
            ppPrec levelApp e1 <+> ppPrec (levelApp + 1) e2
  ppPrec n e@(Fun _ _) =
      parens (levelFun < n) $ pp e
  ppPrec n e@If {} =
      parens (levelIf < n) $ pp e
  ppPrec n e@LetRec {} =
      parens (levelLet < n) $ pp e
  ppPrec _ e' = pp e'
  parens b = if b then PP.parens else id
  -- use the C++ precendence level table
  level :: Bop -> Int
  level Plus   = 3
  level Minus  = 3
  level Times  = 5
  level _      = 8
  levelApp     = 10
  levelIf      = 2
  levelLet     = 1
  levelFun     = 1  -- almost always needs parens
