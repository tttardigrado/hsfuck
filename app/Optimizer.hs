{-# LANGUAGE PatternSynonyms #-}

module Optimizer ( optimizeBF ) where

import Lang ( Op (..), BF, pattern Clear0, pattern Clear1, pattern Mul0, pattern Mul1, pattern Dup0, pattern Dup1)

-- basic pattern for optimization function that covers the "rest" of the cases
basic :: (BF -> BF) -> BF -> BF
basic f ops = case ops of
  []          -> []
  Loop x : xs -> Loop (f x) : f xs -- call inside loops
  x      : xs -> x          : f xs -- move to the next one

-- initial loops will never run
-- [_]... => ...
initLoop :: BF -> BF
initLoop ops = case ops of
  Loop x : xs -> xs
  otherwise   -> ops

-- only the first of sequential loops will execute
-- ...[a][_]... => ...[a]...
deadLoop :: BF -> BF
deadLoop ops = case ops of
  Loop x : Loop _ : xs -> deadLoop $ Loop (deadLoop x) : xs
  otherwise            -> basic deadLoop ops

-- [-] and [+] are used to set the cell to 0
-- ...[-]... => ...0...
zero :: BF -> BF
zero ops = case ops of
  Clear0 : xs -> Set 0 : zero xs
  Clear1 : xs -> Set 0 : zero xs
  otherwise   -> basic zero ops

-- multiple increments/movement can be joined into one
-- ...+++-... => ...++...   ...<<<>... => ...<<...
join :: BF -> BF
join ops = case ops of
  Inc x  : Inc y : xs -> join $ Inc (x+y) : xs
  Mov x  : Mov y : xs -> join $ Mov (x+y) : xs
  otherwise           -> basic join ops

-- multiple set/inp and increment before set/inp are useless
-- ...+++0... => ...0...  ...,0... => ...0...
deadSet :: BF -> BF
deadSet ops = case ops of
  Set _ : Set x : xs -> deadSet $ Set x : xs
  Inc _ : Set x : xs -> deadSet $ Set x : xs
  Inp   : Set x : xs -> deadSet $ Set x : xs
  Set _ : Inp   : xs -> deadSet $ Inp   : xs
  Inc _ : Inp   : xs -> deadSet $ Inp   : xs
  Inp   : Inp   : xs -> deadSet $ Inp   : xs
  otherwise          -> basic deadSet ops

-- moving/increment by zero is useless
deadOp :: BF -> BF
deadOp ops = case ops of
  Inc 0 : xs -> deadOp xs
  Mov 0 : xs -> deadOp xs
  otherwise  -> basic deadOp ops

-- setting followed by incrementing is just setting
-- setting to 0 followed by a loop is just setting to 0
-- ...0+++... => ...3...   ...0[_]... => ...0...
justSet :: BF -> BF
justSet ops = case ops of
  Set x    : Inc y  : xs -> justSet $ Set (x + y) : xs
  Set 0    : Loop _ : xs -> justSet $ Set 0       : xs
  otherwise              -> basic justSet ops

-- [->+<] and [>+<-] are used to multiply to the next value
-- ...[->>+++<<]... => ...*2,3...
mul :: BF -> BF
mul ops = case ops of
  Mul0 x a b : xs | x > 0 && a == -b -> Mul a x : mul xs
  Mul1 x a b : xs | x > 0 && a == -b -> Mul a x : mul xs
  otherwise                          -> basic mul ops

-- [->+>+<<] and [>+>+<<-] are used to copy a value to the next 2
dup :: BF -> BF
dup ops = case ops of
  Dup0 : xs -> Dup : dup xs
  Dup1 : xs -> Dup : dup xs
  otherwise -> basic dup ops


-- final brainfuck optimization function
optimizeBF :: BF -> BF
optimizeBF = deadSet . justSet . deadOp . dup . mul . zero . join . initLoop . deadLoop
