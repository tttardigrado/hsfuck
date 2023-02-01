module Parser ( parseBF ) where

import Text.ParserCombinators.Parsec
    ( char, between, many1, (<|>), parse, ParseError, Parser )
import Lang (Op (..), BF)

infix 5 *: -- fold n c's into an Op (o c)
(*:) :: (Int -> Op) -> Char -> Parser Op
o *: c = fmap (o . length) (many1 $ char c)

infix 5 /: -- fold n c's into an Op (o -c)
(/:) :: (Int -> Op) -> Char -> Parser Op
o /: c = fmap (o . negate . length) (many1 $ char c)

-- parse [ ops ] ==> Loop ops
pLoop :: Parser Op
pLoop = fmap Loop (between (char '[') (char ']') pExpr)

-- parse brainfuck expressions
-- b ::= + | - | < | > | « | » | , | . | 0 | ? | [b] | bb
pExpr :: Parser BF
pExpr = many1
       $  Inc *: '+' 
      <|> Inc /: '-' 
      <|> Mov *: '>' 
      <|> Mov /: '<' 
      <|> Sft *: '»' 
      <|> Sft /: '«' 
      <|> Inp <$ many1 (char ',')
      <|> Out <$ char '.'
      <|> CtR <$ many1 (char '@')
      <|> RtC <$ many1 (char '#')
      <|> Dbg <$ char '?'
      <|> Loop [Inc 1] <$ char '0'
      <|> pLoop

-- non-brainfuck legal characters count as comments and should be ignored
-- filter them out
ignoreComments :: String -> String
ignoreComments = filter (`elem` "+-<>«».,[]0?@#")

-- Parser function for brainfuck programs
parseBF :: String -> Either ParseError BF
parseBF = parse pExpr "" . ignoreComments