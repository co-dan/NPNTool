import Text.Parsec hiding ((<|>))
import Data.Functor.Identity (Identity)
import Text.Parsec.String -- Text.Parsec.Text
import Text.Parsec.Expr
import Control.Applicative
import NCTL

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name fun assoc = Infix (do { string name; return fun }) assoc

exprP :: OperatorTable String () Identity a -> Parser a -> Parser a
exprP table factor = buildExpressionParser table factor
                  <?> "expression"

expr :: Parser NCTL
expr = exprP table factor

table = [ [binary "&&" nctlAnd AssocLeft]
        , [binary "||" NCTLOr  AssocLeft]]
  where nctlAnd a b = NCTLNot $ NCTLOr (NCTLNot a) (NCTLNot b)

factor :: Parser NCTL
factor = spaces *> value <* spaces
  where value = choice [ NCTLTrue <$ string "true"
                       , NCTLFalse <$ string "false"
                       , NCTLNot <$> (char '~' *> factor)
                       , tempModalitiesAr1
                       , tempModalitiesAr2
                       , (char '(' *> spaces) *> expr <* (spaces <* char ')')
                       , fmap NMod $ (char '[' *> spaces) *> expr <* (spaces <* char ']') ]
        tempModalitiesAr1 = foldl1 (<|>) $ map (uncurry ar1Parser) (fst tempModalitiesTable)
        tempModalitiesAr2 = foldl1 (<|>) $ map (uncurry ar2Parser) (snd tempModalitiesTable)

tempModalitiesTable = ( [("EX", EX), ("EF", ef), ("AF", af), ("EG", eg), ("AG", ag)]
                      , [("EU", EU), ("AU", AU)])

  
ar1Parser :: String -> (NCTL -> NCTL) -> Parser NCTL  
ar1Parser s constr = do
  try $ string s
  spaces >> char '('
  e <- expr
  spaces >> char ')'
  return $ constr e

ar2Parser :: String -> (NCTL -> NCTL -> NCTL) -> Parser NCTL
ar2Parser s constr = do
  try $ string s
  spaces >> char '('
  e1 <- expr
  spaces >> char ',' >> spaces
  e2 <- expr
  spaces >> char ')'
  return $ constr e1 e2

