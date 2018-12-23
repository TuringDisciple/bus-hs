module Bus (
      Parser,
      parse, 
      
) where 

import Control.Monad
import Control.Applicative

-- We consider a parser to take in an input string and return parsed output
newtype Parser a = Parser{ parse::String -> [ (String, a) ] }

-- A parser that attempts to get a character from the input stream
item :: Parser Char 
item = Parser(
      \s -> case s of 
            [] -> [] 
            (s':ss) -> [ (ss, s') ]
      )
-- A functor allows us to modify a type that wrapped in a contex
instance Functor Parser where 
      -- fmap :: (a -> b) -> Parser a -> Parser b 
      fmap f (Parser px) = Parser (\s -> [(ss, (f x)) | (ss, x) <- px s])

-- Derived combi
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
(<$) :: a -> Parser b -> Parser a
(<$) x (Parser px) = Parser (\s -> [(ss, x) | (ss, _) <- px s])

