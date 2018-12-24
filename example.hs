import Bus
import Prelude 

{-
      We define a parser for a simple grammar below
      <expr>  := ("+"|"-") <term> ( ("+"|"-") <term> )*
      <term>  := ("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9")+
-}

data Parity = (:+:) | (:-:) deriving (Show)
data Term = Term Int deriving (Show)
data Expr = Expr Parity Term [Expr] deriving (Show)

parity :: Parser Parity
parity =  ((:+:) <$ char '+' <* whitespace )
      <|> ((:-:) <$ char  '-' <* whitespace)

term :: Parser Term
term = Term <$> number <* whitespace

expr :: Parser Expr
expr = Expr <$> parity <*> term <*> many expr <* whitespace
