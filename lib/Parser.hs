module Parser ( parseBF ) where

import Text.ParserCombinators.Parsec
    ( char, between, many1, (<|>), parse, ParseError, Parser )
import Lang (Op (..), BF)

-- parse n +'s  ==> Inc n
pPlus :: Parser Op
pPlus = fmap (Inc . length) (many1 $ char '+')

-- parse n -'s  ==> Inc -n
pMinus :: Parser Op
pMinus = fmap (Inc . negate . length) (many1 $ char '-')

-- parse n >'s  ==> Mov n
pRight :: Parser Op
pRight = fmap (Mov . length) (many1 $ char '>')

-- parse n <'s  ==> Mov -n
pLeft :: Parser Op
pLeft = fmap (Mov . negate . length) (many1 $ char '<')

-- parse . ==> Out
pOut :: Parser Op
pOut = Out <$ char '.'

-- parse , ==> Inp
pInp :: Parser Op
pInp = Inp <$ char ','

-- parse # ==> Debug
pDebug :: Parser Op
pDebug = Debug <$ char '#'

-- parse [ ops ] ==> Loop ops
pLoop :: Parser Op
pLoop = fmap Loop (between (char '[') (char ']') pExpr)

-- parse brainfuck expressions
-- <bf> ::= + | - | . | , | < | > | [ <bf> ] | <bf> <bf>
pExpr :: Parser BF
pExpr = many1 $ pPlus <|> pMinus <|> pRight <|> pLeft <|> pInp <|> pOut <|> pDebug <|> pLoop

-- non-brainfuck legal characters count as comments and should be ignored
-- filter them out
ignoreComments :: String -> String
ignoreComments = filter (`elem` "+-<>.,[]#")

-- Parser function for brainfuck programs
parseBF :: String -> Either ParseError BF
parseBF = parse pExpr "" . ignoreComments