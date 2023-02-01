{-# LANGUAGE PatternSynonyms #-}

module Lang (Op (..), BF, pattern Clear0, pattern Clear1, pattern Mul0, pattern Mul1, pattern Dup0, pattern Dup1) where

-- Representation of an individual brainfuck operation
data Op
  = Inc Int     -- + -    inc/dec current by amount
  | Mov Int     -- < >    move pointer by ammount
  | Sft Int     -- « »    right/left shift
  | Out         -- .      output
  | Inp         -- ,      input
  | Set Int     -- OPTIM: set current to value
  | Mul Int Int -- OPTIM: set displacement cell to a multiple
  | Dup         -- OPTIM: duplicate to the two next values
  | Dbg         -- DEBUG: construct: print 10 cells 
  | Loop BF     -- [...]  loop the inner ops
  deriving (Show, Eq)


-- A Sequence of operations is a brainfuck program
type BF = [Op]

-- Patterns synonyms
pattern Clear0 :: Op
pattern Clear0 <- Loop[Inc (-1)]
pattern Clear1 :: Op
pattern Clear1 <- Loop[Inc 1]

pattern Mul0 :: Int -> Int -> Int -> Op
pattern Mul0 x a b <- Loop [Inc (-1), Mov a, Inc x, Mov b]
pattern Mul1 :: Int -> Int -> Int -> Op
pattern Mul1 x a b <- Loop [Mov a, Inc x, Mov b, Inc (-1)]

pattern Dup0 :: Op
pattern Dup0 <- Loop [Inc (-1), Mov 1, Inc 1, Mov 1, Inc 1, Mov (-2)]
pattern Dup1 :: Op
pattern Dup1 <- Loop [Mov 1, Inc 1, Mov 1, Inc 1, Mov (-2), Inc (-1)]