module Generator ( generateC ) where

import Lang ( Op (..), BF )

-- generate a string of n tabs
tabs :: Int -> String
tabs n = replicate n '\t'

-- convert a single operation op into it's C equivalent idented by n
opToC :: Int -> Op -> String
opToC n op = case op of
  Inc x       -> concat [tabs n,  "*ptr += ", show x, ";\n"]
  Sft x | x>0 -> concat [tabs n, "*ptr >>= ", show x, ";\n"]
  Sft x       -> concat [tabs n, "*ptr <<= ", show x, ";\n"]
  Mov x       -> concat [tabs n,   "ptr += ", show x, ";\n"]
  Out         -> tabs n ++ "putchar(*ptr);\n"
  Inp         -> tabs n ++ "*ptr = getchar();\n"
  Set x       -> concat [tabs n, "*ptr = ",  show x, ";\n"]
  Mul d x     -> concat [tabs n, "mul(", show d, ", ", show x, ");\n"]
  Dup         -> tabs n ++ "dup();\n"
  Dbg         -> tabs n ++ "debug();\n"
  Loop xs     -> concat
    [ tabs n, "while (*ptr) {\n"
    , bfToC (n+1) xs
    , tabs n, "}\n"
    ]

-- convert a brainfuc program to C starting with a given identation
bfToC :: Int -> BF -> String
bfToC = concatMap . opToC

-- full C code generation function
-- translate the code and create the tape and pointer
generateC :: BF -> String
generateC bf = concat
  [ "#include <stdio.h>\n"
  , "#define mul(d, m) *(ptr+(d)) += *ptr * (m); *ptr = 0;\n"
  , "#define debug() printf(\"\\n# DEBUG: | \");for(int i=0;i<10;i++){printf(\"%d | \", *(ptr+i));}printf(\"\\n\\n\");\n"
  , "#define dup() *(ptr+1) += *ptr; *(ptr+2) += *ptr; *ptr = 0;\n"
  , "\nint main(void) {\n"
  , "\tchar tape[30000] = {0};\n"
  , "\tchar *ptr = tape;\n\n"
  , bfToC 1 bf
  , "\n}"
  ]