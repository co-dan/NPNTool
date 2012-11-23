import Text.Parsec hiding ((<|>))
import Data.Functor.Identity (Identity)
import Text.Parsec.String -- Text.Parsec.Text
import Text.Parsec.Expr
import Control.Applicative
import NCTL

type Nctl = NCTL String String

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name fun assoc = Infix (do { string name; return fun }) assoc

exprP :: OperatorTable String () Identity a -> Parser a -> Parser a
exprP table factor = buildExpressionParser table factor
                  <?> "expression"

expr :: Parser (NCTL String String)
expr = exprP table factor

exprNested :: Parser (NNCTL String String)
exprNested = exprP tableNested factorNested

table = [ [binary "&&" nctlAnd AssocLeft]
        , [binary "||" NCTLOr  AssocLeft]]
  where nctlAnd a b = NCTLNot $ NCTLOr (NCTLNot a) (NCTLNot b)

tableNested = [ [binary "&&" nctlAnd AssocLeft]
              , [binary "||" NNCTLOr  AssocLeft]]
  where nctlAnd a b = NNCTLNot $ NNCTLOr (NNCTLNot a) (NNCTLNot b)        
        
factor :: Parser (NCTL String String)
factor = spaces *> value <* spaces
  where value = choice [ NCTLTrue <$ string "true"
                       , NCTLFalse <$ string "false"
                       , NCTLNot <$> (char '~' *> factor)
                       , tempModalities
                       , (char '(' *> spaces) *> expr <* (spaces <* char ')')
                       , fmap Nmod $ (char '[' *> spaces) *> exprNested <* (spaces <* char ']') ]
        tempModalities = exParser <|> euParser

factorNested :: Parser (NNCTL String String)
factorNested = spaces *> value <* spaces
  where value = choice [ NNCTLTrue <$ string "true"
                       , NNCTLFalse <$ string "false"
                       , NNCTLNot <$> (char '~' *> factorNested)
                       , tempModalities
                       , (char '(' *> spaces) *> exprNested <* (spaces <* char ')')]
        tempModalities = exParserN <|> euParserN

-- TODO: any tricks for code reuse?
exParser :: Parser (NCTL String String)                     
exParser = do  
  try $ string "EX"
  spaces >> char '(' 
  e <- expr
  spaces >> char ')'
  return $ EX e

euParser :: Parser (NCTL String String)
euParser = do
  try $ string "EU"
  spaces >> char '('
  e1 <- expr
  spaces >> char ',' >> spaces
  e2 <- expr
  spaces >> char ')'
  return $ EU e1 e2

exParserN :: Parser (NNCTL String String)                     
exParserN = do  
  try $ string "EX"
  spaces >> char '(' 
  e <- exprNested
  spaces >> char ')'
  return $ NEX e

euParserN :: Parser (NNCTL String String)
euParserN = do
  try $ string "EU"
  spaces >> char '('
  e1 <- exprNested
  spaces >> char ',' >> spaces
  e2 <- exprNested
  spaces >> char ')'
  return $ NEU e1 e2
