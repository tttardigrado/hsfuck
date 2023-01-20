---
title: "Build you a brainfuck compiler for the greater good"
date: 2023-01-19T00:35:08Z
summary: "How I built an optimizing `brainfuck` to `C` compiler using Haskell, and you could too."
tags: ["haskell", "esolangs", "compilers"]
---

I `love` brainfuck. Most of my friends probably hate it because, sometimes, I talk wayyyyyy too much about it... but I love brainfuck.

It's an exercise on simplicity, design, and, honestly, quite fun. Since I learned about brainfuck, I've implemented interpreters in several languages, written small programs, and designed [languages inspired by it](https://tttardigrado.github.io/posts/mixtape/), but there was something that I had not yet crossed out of my to-do list: a brainfuck compiler.

That changes today, and I'm building it using Haskell (because I can and I need to practice Haskell for my functional programming course next semester)

The full code is available at this [repo](https://github.com/tttardigrado/hsfuck).

## Index
1. [What is brainfuck?](#what-is-brainfuck)
1. [Parsing](#parsing)
2. [Optimization](#optimization)
3. [Code Generation](#code-generation)
4. [Conclusion](#conclusion)

## What is brainfuck?

brainfuck was created in 1993 by Urban Müller. Inspired by FALSE's 1024-byte compiler, Müller wanted to create a smaller one. The language consists of a `tape` (an array of byte-sized cells), a `head` (a pointer to the current cell) and instructions that manipulate them:

| Instruction | Meaning                            |
|-------------|------------------------------------|
| +           | increment the current cell         |
| -           | decrement the current cell         |
| <           | move the head left                 |
| >           | move the head right                |
| .           | print the current cell's as ASCII  |
| ,           | set the current cell to user input |
| [ ]         | while loop                         |

The `tape` is represented as an array with a length of `30000` and the `head` is a pointer to it. In C:

```c
char tape[30000] = {0};
char *ptr = tape;
```

Even if brainfuck is simple, it was proved to be [Turing-Complete](http://www.iwriteiam.nl/Ha_bf_Turing.html), proving, one more time, that complexity can emerge from simplicity.

If you want to dig deeper into this rabbit hole, [Wikipedia](https://en.wikipedia.org/wiki/Brainfuck)'s](https://en.wikipedia.org/wiki/Brainfuck) and [Esolangs'](https://esolangs.org/wiki/Brainfuck) articles are good places to start.

## Parsing

We can't process something we don't know. This means that, before parsing, we should understand what we are parsing and into what we are parsing it. We'll parse brainfuck into the `BF` type:

```haskell
data Op
  = Inc Int -- + -
  | Mov Int -- < >
  | Out     -- .
  | Inp     -- ,
  | Loop BF -- [ ]
  deriving (Show, Eq)

type BF = [Op]
```

Now that we comprehend the output, we must understand the input. Brainfuck's syntax is simple and is described by the following (equally simple) grammar:

```bash
<bf> ::= + | - | < | > | . | , | [<bf>] | <bf><bf>
```

This representation is called [Backus-Naur-Form (BNF)](https://matt.might.net/articles/grammars-bnf-ebnf/). If you don't know how to interpret *BNF*, it essentially says that a valid program is one that can be obtained by successively expanding `<bf>` into one of that patterns. This means that `+++[>]` is valid, while `+++[` is not.

Knowing both the input and the output, the only missing piece (arguably the most important) is the tool that performs the translation between them: the **parser**. We're going to use [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) to perform this task, since they are simple and allow our parser to be similar to the grammar presented above. If the word *Monad* scares you, don't worry, it also scares me, and I'm not going to try to explain category theory to you (at least, not today).

Let's start by building 6 elementary parsers (for `+`, `-`, `<`, `>`, `.` and `,`)

```haskell
-- All of them have the same type
pPlus, pMinus, pRight, pLeft, pInp, pOut :: Parser Op

-- (fmap f p) means that we will parse using the Parser p
--    and apply the function f to it's content
--    ex: pPlus parses at least one + and returns
--        the number of parsed characters wrapped in Inc
pPlus  = fmap (Inc . length)          (many1 $ char '+')
pMinus = fmap (Inc . negate . length) (many1 $ char '-')
pRight = fmap (Mov . length)          (many1 $ char '>')
pLeft  = fmap (Mov . negate . length) (many1 $ char '<')

-- (a <$ p) means that we will parse using the Parser p
--    and on success return a
pInp = Inp <$ char ','
pOut = Out <$ char '.'
```

The parsers for a loop and a full program can also be implemented in the following way:

```haskell
-- map a match of [ pExpr ] into Loop
pLoop :: Parser Op
pLoop = fmap Loop (between (char '[') (char ']') pExpr)

-- <|> means option, similar to | in the grammar
pExpr :: Parser BF
pExpr :: many1 (pPlus <|> pMinus <|> pRight <|> pLeft <|> pInp <|> pOut <|> pLoop)
```
Now, we just need to create a function that ignores non-brainfuck characters (since they are treated as comments) and uses `pExpr` to parse programs. Voila, we have a working parser.

```haskell
-- return only valid bf characters
ignoreComments :: String -> String
ignoreComments = filter (`elem` "+-<>.,[]")

-- ignore comments and try to parse the program
parseBF :: String -> Either ParseError BF
parseBF = parse pExpr "Parser" . ignoreComments
```

## Optimization

The next part of the compilation process is the optimization phase. It consists of the manipulation of the data returned by the parser to make the program better. There are numerous ways to optimize brainfuck programs, and I'm not trying to cover them all in this post.

We'll look at 3 kinds of optimizations: dead code elimination, combining instructions and abstracting common patterns.

### Dead Code Elimination

Sometimes (actually, a lot of times) we write code that will never run. It could be code after a return statement, an if statement whose condition is always false, etc... brainfuck also has that problem, mostly when talking about loops.

Loops only run if the cell has a `non-zero` value and only exit when the cell has a `zero` value. This means that if we have multiple loops in a row, only the first one will be executed (when the program exits that loop, the cell is set to zero, so no other loop will run).

```
[+++>][.-][+<+>]     <=>     [+++>]
```

Because of this property, we can remove these loops, reducing the amount of code that gets generated.

```haskell
deadLoop :: BF -> BF
deadLoop ops = case ops of
  [] -> []
  -- remove multiple loops, keep the first
  Loop x : Loop _ : xs -> deadLoop (Loop x : xs)
  -- also look for this pattern inside nested loops
  Loop x : xs          -> Loop (deadLoop x) : xs
  x      : xs          -> x : deadLoop xs                    
```

### Combining Instructions

Some instructions can be optimized by doing a lot in fewer steps. Two obvious examples are `Inc` and `Mov` instructions. Ex: incrementing by `10` and then decrementing by `4` is the same as just incrementing by `6`.

Some of the work has already been done by the parser, when it combines a sequence of instructions into a single one, i.e. `>>>>` into `Mov 4`, but this is not enough. `+++--` is still parsed into `[Inc 3, Inc (-2)`, but it could be optimized into `[Inc 1]`.

```haskell
[Inc 3, Inc (-2)]  ==>  [Inc (3 - 2)]  ==>  [Inc 1]
```

This can be achieved by joining successive `Inc` and `Mov` instructions as an optimization pass.

```haskell
join :: BF -> BF
join ops = case ops of
  []                  -> []
  -- join multiple Inc instructions
  Inc x  : Inc y : xs -> join (Inc (x+y) : xs)
  -- join mltiple Mov instructions
  Mov x  : Mov y : xs -> join (Mov (x+y) : xs)
  -- also look for this pattern inside nested loops
  Loop x : xs          -> Loop (join x) : xs
  x      : xs          -> x : join xs
```

### Abstracting Patterns

Brainfuck's simplicity makes it necessary to use multiple operations to perform some simple things. Over the years, some patterns that execute those operations have been found, but they are usually very inefficient. Some examples are:

* `[-]` and `[+]` to set a cell to 0
* `[->+<]` to move a value from the current cell to the next
* `[->++<]` to move a value from the current cell to the next and duplicate it
* `[->+>+<<]` to move a value from the current cell to the next two
* ...

This optimization method is based on identifying these patterns and abstracting them into a new `pseudo-operation`. We are going to exemplify this with the `[-]` pattern.

The first step is to extend our `Op` type with a new constructor, called `Clear`, that represents this pattern.

```haskell
data Op
  ...
  | Clear
  deriving (Show, Eq)
```

Now we just need a simple function that looks for this pattern. If it finds it, abstract it. If not, just move on:

```haskell
clear :: BF -> BF
clear ops = case ops of
  []                   -> []
  Loop [Inc (-1)] : xs -> Clear : clear xs -- [-]
  Loop [Inc   1 ] : xs -> Clear : clear xs -- [+]
  -- also look for clear patterns inside nested loops
  Loop x : xs          -> Loop (clear x) : xs -- [...[-]...]
  x               : xs -> x : clear xs
```

### The final optimization

The last thing we need is a single `optimize` function that runs these individual optimization passes in a coherent order (which is, sometimes, very difficult to figure out).

```haskell
optimize :: BF -> BF
optimize = clear . join . deadLoop
```

## Code Generation

Until now, we have been talking about parsing and optimization, but all of that is kinda useless if we can't run our code. To make it runnable, we're going to compile brainfuck down to C. (I chose C to simplify the process, but a good exercise would be to compile it down to some assembly language, python bytecode or java bytecode)

| Op      | C equivalent          |
|---------|-----------------------|
| Inc n   | `*ptr += n;`          |
| Mov n   | `ptr += n;`           |
| Out     | `putchar(*ptr);`      |
| Inp     | `*ptr = getchar();`   |
| Clear   | `*ptr = 0;`           |
| Loop xs | `while (*ptr) { xs }` |

brainfuck's semantics are identical to *C*'s, making the translation process between them straightforward. Before the translation, we need a working *C* program with some auxiliary structures: the **Tape** and the **Pointer** (as shown above)

```c
#include <stdio.h>

int main(void) {
    // create the tape with 30000 cells and the pointer
    char tape[30000] = {0};
    char *ptr = tape;

    // generated brainfuck code
}
```

With the structure of the *C* program out of the way, we can start to define our code generator. We'll start with a function that converts brainfuck commands into their equivalent *C* statement (with a given indentation, `n`, for pretty printing purposes).

```haskell
-- generate a string of n tabs for indentation purposes
tabs :: Int -> String
tabs n = replicate n '\t'

opToC :: Int -> Op -> String
opToC n op = case op of
  Inc x -> concat [tabs n, "*ptr += ", show x, ";\n"] -- *ptr += x;
  Mov x -> concat [tabs n,  "ptr += ", show x, ";\n"] -- ptr += x;
  Out   -> tabs n ++ "putchar(*ptr);\n"               -- output
  Inp   -> tabs n ++ "*ptr = getchar();\n"            -- set to input
  Clear -> tabs n ++ "*ptr = 0;\n"]                   -- set to zero
```

There is a little issue: `opToC` only translates a single `Op`, but both our loops and our programs are lists of `Op`. To solve this, we need a new function that maps every `Op` to its translation and concatenates the results. (While we're at it, let's use it to implement loops)

```haskell
opToC :: Int -> [Op] -> String
opToC n op = case op of
  ...
  Loop xs -> concat
    [ tabs n, "while (*ptr) {\n"
    -- map the inner ops with incremented identation
    , bfToC (n+1) xs
    , tabs n, "}\n"
    ]

-- map every op (with identation n) and concatenate the result
bfToC :: Int -> [Op] -> String
bfToC n ops = concatMap (opToC n) ops
```

The last thing needed for a fully functioning code generator is a `generateC` function that generates the C program we saw previously.

```haskell
generateC :: [Op] -> String
generateC bf = concat
  [ "#include <stdio.h>\n"
  , "int main(void) {\n"
  , "\tchar tape[30000] = {0};\n"
  , "\tchar *ptr = array;\n\n"
  , bfToC 1 bf
  , "\n}"
  ]
```

## Conclusion

brainfuck is the perfect language to write a compiler for.

It's simple to parse, easy to find basic optimizations, and has an almost one-to-one translation to C making it easy to generate code. What I've shown here is only a fragment of what a brainfuck compiler could become. I hope this article interested you in the art of writing brainfuck compilers and that it inspires you to try to implement your own, with higher-level language constructs, new and cleverer optimizations, and more target languages such as web assembly, x86, or the JVM.


---

`Notes for Hasklers`:

1. I know I'm using way too many parentheses when I should just use `$`, but I'm trying to make the code readable for people who are new to Haskell.

2. There's probably some obscure Monad or functional programming technique that would have simplified the code. Please check the git repo and make a pull request or raise an issue. You can also just message me on Instagram or Twitter, I'd love to talk about Haskell and functional programming.