module Generator ( generateC ) where

import Lang ( Op (..), BF )

-- generate a string of n tabs
tabs :: Int -> String
tabs n = replicate n '\t'

-- convert a single operation op into it's C equivalent idented by n
opToC :: Int -> Op -> String
opToC n op = case op of
  Inc x   -> concat [tabs n, "*ptr += ", show x, ";\n"]
  Mov x   -> concat [tabs n,  "ptr += ", show x, ";\n"]
  Out     -> tabs n ++ "putchar(*ptr);\n"
  Inp     -> tabs n ++ "*ptr = getchar();\n"
  Set x   -> concat [tabs n, "*ptr = ",  show x, ";\n"]
  Debug   -> tabs n ++ "DEBUG();\n"
  Loop xs -> concat
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
  , "#define DEBUG() printf(\"\\n# DEBUG: | \");for(int i=0;i<10;i++){printf(\"%d | \", *(ptr+i));}printf(\"\\n\\n\");\n"
  , "int main(void) {\n"
  , "\tchar tape[30000] = {0};\n"
  , "\tchar *ptr = tape;\n\n"
  , bfToC 1 bf
  , "\n}"
  ]