# Code Generation

Until now, we have been talking about parsing and optimization, but all of that is kinda useless if we can't run our code. To make it runnable, we're going to compile brainfuck down to C. (I chose C to simplify the process, but a good exercise would be to compile it down to some assembly language, python bytecode or java bytecode)

brainfuck's semantics are identical to C's, making the translation process between them straightforward. Before the translation, we need a working C program with some auxiliary structures: the Tape and the Pointer (as explained above)

```c
#include <stdio.h>

int main(void) {
    // create the tape with 30000 cells and the pointer
    char tape[30000] = {0};
    char *ptr = tape;

    // generated brainfuck code
}
```

With the structure of the C program out of the way, we can start to define our code generator. We'll start with a function that converts brainfuck commands into their equivalent C statement (with a given indentation, `n`, for pretty printing purposes).

```haskell
-- generate a string of n tabs for indentation purposes
tabs :: Int -> String
tabs n = replicate n '\t'

opToC :: Int -> Op -> String
opToC n op = case op of
  Inc x   -> concat [tabs n, "*ptr += ", show x, ";\n"] -- *ptr += x;
  Mov x   -> concat [tabs n,  "ptr += ", show x, ";\n"] --  ptr += x;
  Out     -> tabs n ++ "putchar(*ptr);\n"
  Inp     -> tabs n ++ "*ptr = getchar();\n"
  Set x   -> concat [tabs n, "*ptr = ",  show x, ";\n"] --  *ptr = x;
```

There is a little issue: `opToC` only translates a single `Op`, but both our loops and our programs are lists of `Op`. To solve this, we need a new function that maps every `Op` to its translation and concatenates the results. (While we're at it, let's use it to implement loops)

```haskell
opToC :: Int -> [Op] -> String
opToC n op = case op of
  ...
  Loop xs -> concat
    [ tabs n, "while (*ptr != 0) {\n"
    -- map the inner ops with incremented identation
    , bfToC (n+1) xs
    , tabs n, "}\n"
    ]

-- map every op (with identation n) and concatenate the result
bfToC :: Int -> [Op] -> String
bfToC n ops = concatMap (opToC n) ops
```

The last thing needed for fully functioning code generation is a `generate` function that generates the C program we saw previously.

```haskell
generateC :: [Op] -> String
generateC bf = concat
  [ "#include <stdio.h>\n"
  , "int main(void) {\n"
  , "\tchar array[30000] = {0};\n"
  , "\tchar *ptr = array;\n\n"
  , bfToC 1 bf
  , "\n}"
  ]
```