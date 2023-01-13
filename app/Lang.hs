module Lang ( Op (..), BF ) where

-- Representation of an individual brainfuck operation
data Op
  = Inc Int   -- + -    inc/dec current by amount
  | Mov Int   -- < >    move pointer by ammount
  | Out       -- .      output
  | Inp       -- ,      input
  | Set Int   -- OPTIM: set current to value
  | Mul Int   -- OPTIM: set the next cell to a multiple
  | Debug     -- DEBUG: construct: print 10 cells 
  | Loop [Op] -- [...]  loop the inner ops
  deriving (Show, Eq)


-- A Sequence of operations is a brainfuck program
type BF = [Op]