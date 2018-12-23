{--
          _____                    _____                    _____
         /\    \                  /\    \                  /\    \
        /::\    \                /::\____\                /::\    \
       /::::\    \              /:::/    /               /::::\    \
      /::::::\    \            /:::/    /               /::::::\    \
     /:::/\:::\    \          /:::/    /               /:::/\:::\    \
    /:::/__\:::\    \        /:::/    /               /:::/__\:::\    \
   /::::\   \:::\    \      /:::/    /                \:::\   \:::\    \
  /::::::\   \:::\    \    /:::/    /      _____    ___\:::\   \:::\    \
 /:::/\:::\   \:::\ ___\  /:::/____/      /\    \  /\   \:::\   \:::\    \
/:::/__\:::\   \:::|    ||:::|    /      /::\____\/::\   \:::\   \:::\____\
\:::\   \:::\  /:::|____||:::|____\     /:::/    /\:::\   \:::\   \::/    /
 \:::\   \:::\/:::/    /  \:::\    \   /:::/    /  \:::\   \:::\   \/____/
  \:::\   \::::::/    /    \:::\    \ /:::/    /    \:::\   \:::\    \
   \:::\   \::::/    /      \:::\    /:::/    /      \:::\   \:::\____\
    \:::\  /:::/    /        \:::\__/:::/    /        \:::\  /:::/    /
     \:::\/:::/    /          \::::::::/    /          \:::\/:::/    /
      \::::::/    /            \::::::/    /            \::::::/    /
       \::::/    /              \::::/    /              \::::/    /
        \::/____/                \::/____/                \::/    /
         ~~                       ~~                       \/____/

 --                         A parser library
 --               Author:Nashe Mncube/TuringDisciple
 --}
module Bus (
      Parser,
      parse,

      -- Functor
      (<$>), (<$),

      -- Applicative
      -- pure, (<*>), (<*), (*>),
) where

import Prelude hiding ((<$), (<$>))
import Control.Monad
import Control.Applicative hiding ((<$), (<$>), many, some)

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
(<$) :: Functor f => a -> f b -> f a
(<$) = fmap.const

-- An applicative encapsulates the concept of applying a wrapped function to wrapped context
instance Applicative Parser where
      -- pure :: a -> Parser a
      pure x = Parser (\s -> [(s, x)])
      -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
      (<*>) (Parser pf) (Parser px) = Parser(\s ->
            [ (sss, f x)| (ss, f) <- pf s, (sss, x) <- px ss])

-- Derived combi
(<*) :: Applicative f => f a -> f b -> f a
px <* py = const <$> px <*> py
(*>) :: Applicative f => f a -> f b -> f b
px *> py = id <$ px <*> py

-- An alternative represents the idea of having multiple parses
instance Alternative Parser where
      -- empty :: Parser a
      empty = Parser(\ts -> [])

      -- <|> :: Parser a -> Parser a -> Parser a
      Parser px <|> Parser py = Parser (\s -> (px s) ++ (py s))

-- Derived combi
(<:>) :: Alternative f => f a -> f [a] -> f [a]
x <:> xs = (:) <$> x <*> xs

some :: Alternative f => f a -> f [a]
some px = px <:> many px

many :: Alternative f => f a -> f [a]
many px = some px <|> empty

-- A monade allows us to chain, and therefore encapsulates a modifiable state
instance Monad Parser where
      -- return :: a -> Parser a
      return = pure

      -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
      (Parser px) >>= f = Parser(\s -> concat [parse (f x) ss | (ss, x) <- px s])
