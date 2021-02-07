module Parser where
import Control.Applicative
import Data.Char

{- | Todos estas funciones salieron de los vídeos de Manu
Inicia aquí---------------------------------------------
-}


newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
    fmap f p = P(\inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> [(f v, out)])

instance Applicative Parser where
    pure v = P (\inp -> [(v,inp)])

    pg <*> px = P (\ inp -> case parse pg inp of
                    [] -> []
                    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> parse (f v) out)

instance Alternative Parser where

    empty = failure
    a <|> b = P (\inp -> case parse a inp of
                            [] -> parse b inp
                            [(v,out)] -> [(v,out)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\ inp-> [])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (x==)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphaNum
  return (x:xs)

nat :: Parser Int
nat = do
  x <- some digit
  return (read x)

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int = do
  char '-'
  n <- nat
  return (-n)
  <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v


identifier :: Parser String
identifier = do token ident

natural :: Parser Int
natural = do token nat

integer :: Parser Int
integer = do token int


symbol :: String -> Parser String
symbol xs = token (string xs)

{- | Acá terminan las funciones creadas por Manu
-}

{- | Esta es la gramática que seguimos:

expr ::= term addop expr | term
term ::= factor mulop term | factor
factor ::= digit | ( expr )
digit ::= 0 | 1 | ... | 9
addop ::= + | -
mulop ::= * | /
-}


{- | Regresa un Parser Int, es decir, una función
que recibe un string y regresa una tupla [(Int, String)]
Se hace de esta manera para que se pueda pasar a la función
parse.
Siguiendo la gramática tenemos dos opciones para leer:
 - Leer un  << term addop expr >>
 - Leer un << term >>
Después de leer un term trata de leer un símbolo de "+" y
regresa la suma de t1 y t2.
si no lo consigue, regresa a leer el puro término. Como falló,
no se hace la "asignación" de t1.
De igual forma, si no consigue leer el símbolo "-", regresa [].

-}
expr :: Parser Int
expr =
  do
  t1 <- term
  do
      symbol "+"
      t2 <- expr
      return (t1 + t2)
      <|>
      do
        symbol "-"
        t2 <- expr
        return (t1 - t2)
  <|>
  term

{- | Siguiendo la gramática, primero trata de leer un factor y
después un símbolo "*" o un símbolo "/" para hacer la
operación correspondiente y regresarla. (Como trabajamos con entero
en lugar de una división "/" se hace una división entera div).
Si falla en leer "*" o en leer "/", entonces se regresa a leer
el puro factor.
-}
term :: Parser Int
term =
  do
  t1 <- factor
  do
    symbol "*"
    t2 <- expr
    return (t1 * t2)
    <|>
    do
      symbol "/"
      t2 <- expr
      return (div t1 t2)
  <|>
  factor

{- | Intenta leer un entero. Si no lo consigue, intenta leer un
"("  expr ")".
-}
factor :: Parser Int
factor = do
  int
  <|>
  do
    symbol "("
    t1 <- expr
    symbol ")"
    return t1

{- | Primero obtiene una línea de la terminal.
Después parsea esa x usando el parser expr.
Ya que obtiene el resultado, lo transforma a una cadena.
Ya que es una cadena, la imprime en pantalla.
Cuando termina, vuelve a llamarse a sí misma para mantener el loop.
Para cancelarlo hay que aplicarle el ctrl + c
-}

main :: IO ()
main = do
    entrada <- getLine
    let c = parse expr entrada
    if c == []
      then
        putStrLn "Expresión inválida"
      else
        putStrLn $ (show (fst (head c))) ++ "\n"
    main
