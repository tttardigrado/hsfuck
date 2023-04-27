module GenMIPS ( generateMIPS ) where

import Lang ( IOKind (..), Op (..), BF )

printMIPS :: Int -> String
printMIPS code = concat
  [ "\t\tli   $v0, ", show code, "\n"
  , "\t\tmove $a0, $s1\n"
  , "\t\tsyscall\n"
  ]

readMIPS :: Int -> String
readMIPS code = concat
  [ "\t\tli   $v0, ", show code, "\n"
  , "\t\tsyscall\n"
  , "\t\tmove $s1, $v0\n"
  ]

loopMIPS :: Int -> BF -> String
loopMIPS n body = concat
  [ "loop" ++ show n ++ ": beqz $s1, end" ++ show n ++ "\n"
  , bfToMIPS (n+1) body
  , "end" ++ show n ++ ":\n"
  ]

moveMIPS :: Int -> String
moveMIPS n = concat 
  [ "\t\tsw   $s1, 0($s0)\n"
  , "\t\taddi $s0, $s0, ", show (n * 4), "\n"
  , "\t\tlw   $s1, 0($s0)\n"
  ]

mulMIPS :: Int -> Int -> String
mulMIPS d m = concat
  [ "\t\tmul  $t0, $s1, ", show m, "\n"
  , "\t\tlw   $t1, ", show (d * 4), "($s0)\n"
  , "\t\taddi $t1, $t1, $t0\n"
  , "\t\tsw   $t1, ", show (d * 4), "($s0)\n"
  ]

-- $s0 -> ptr    $s1 -> *ptr
-- convert a single operation op into it's C equivalent idented by n
opToMIPS :: Int -> Op -> String
opToMIPS n op = case op of
  Inc x       -> "\t\taddi $s1, $s1, " ++ show x ++ "\n"
  Sft x | x>0 -> "\t\tslr  $s1, $s1, " ++ show x ++ "\n"
  Sft x       -> "\t\tsll  $s1, $s1, " ++ show x ++ "\n"
  Mov x       -> moveMIPS  x
  Out KChr    -> printMIPS 11 
  Out KNum    -> printMIPS 1
  Inp KChr    -> readMIPS  12
  Inp KNum    -> readMIPS  5
  Set x       -> "\t\tli   $s1, " ++ show x ++ "\n"
  Mul d x     -> mulMIPS d x
  Dup         -> "\t\tsw   $s1, 4($s0)\n\t\tsw   $s1, 8($s0)\n\t\tli   $s1, 0\n"
  Dbg         -> undefined
  Loop xs     -> loopMIPS n xs

-- convert a brainfuck program to MIPS starting with a given Loop nesting level
bfToMIPS :: Int -> BF -> String
bfToMIPS = concatMap . opToMIPS

-- full MIPS code generation function
-- translate the code and create the tape and pointer
generateMIPS :: BF -> String
generateMIPS bf = concat
  [ "\t\t.data\n"
  , "arr:    .word 0:30000\n\n"
  , "\t\t.text\n"
  , "main:\n"
  , "\t\tla   $s0, arr\n"
  , "\t\tli   $s1, 0\n\n"
  , bfToMIPS 0 bf
  , "\n\t\tli   $v0, 10\n"
  , "\t\tsyscall"
  ]