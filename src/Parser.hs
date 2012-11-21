import Text.Parsec hiding ((<|>))
import Data.Functor.Identity (Identity)
import Text.Parsec.String -- Text.Parsec.Text
import Text.Parsec.Expr
import Control.Applicative
import NCTL

type Nctl = NCTL String String


expr :: Parser (NCTL String String)
expr = buildExpressionParser table factor
   <?> "expression"

table = [ [binary "&&" nctlAnd AssocLeft]
        , [binary "||" NCTLOr  AssocLeft]]
  where nctlAnd a b = NCTLNot $ NCTLOr (NCTLNot a) (NCTLNot b)

factor :: Parser (NCTL String String)
factor = spaces *> value <* spaces
  where value = choice [ NCTLTrue <$ string "true"
                       , NCTLFalse <$ string "false"
                       , NCTLNot <$> (char '~' *> factor)
                       , tempModalities
                       , (char '(' *> spaces) *> expr <* spaces]
        tempModalities = exParser <|> euParser


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

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name fun assoc = Infix (do { string name; return fun }) assoc
