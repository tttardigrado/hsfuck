module Lang ( Op (..), BF ) where

-- Representation of an individual brainfuck operation
data Op
  = Inc Int     -- + -    inc/dec current by amount
  | Mov Int     -- < >    move pointer by ammount
  | Out         -- .      output
  | Inp         -- ,      input
  | Set Int     -- OPTIM: set current to value
  | Mul Int Int -- OPTIM: set displacement cell to a multiple
  | Dup         -- OPTIM: duplicate to the two next values
  | Debug       -- DEBUG: construct: print 10 cells 
  | Loop BF     -- [...]  loop the inner ops
  deriving (Show, Eq)


-- A Sequence of operations is a brainfuck program
type BF = [Op]