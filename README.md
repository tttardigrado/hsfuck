# hsfuck
![Tests CI](https://github.com/tttardigrado/hsfuck/actions/workflows/tests.yml/badge.svg)
![Build CI](https://github.com/tttardigrado/hsfuck/actions/workflows/haskell.yml/badge.svg)
![License](https://img.shields.io/github/license/tttardigrado/hsfuck)
<a href="https://twitter.com/intent/tweet?text=Check%20out%20hsfuck%20by%20%40_tardigrado_%20https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fhsfuck 😁"><img src="https://img.shields.io/twitter/url?style=social&url=https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fhsfuck"></a>

![Logo](./Logo.png)

A _brainfuck_ compiler written in _Haskell_

## Tech stack
* Languages: Haskell
* Packages: Parsec

## Blog Post
I wrote a [blog post](https://tttardigrado.github.io/posts/hsfuck/) about this project

## How to install and use
You need to have **cabal**, **Haskell** installed. Then run the following commands
To run the program you need **gcc** for the **C** version and **SPIM** for the **MIPS** version

```sh
# clone the repo and move to it
git clone https://github.com/tttardigrado/hsfuck
cd hsfuck

# build the project using cabal
cabal build

# optionally move the binary into another location with
# cp ./path/to/binary .

# run the compiler
# (fst argument is compilation target mode. Either c or mips)
# (snd argument is the path of the src file)
# (trd argument is the path of the output file)
./hsfuck c test.bf test.c

# compile and run the C code
gcc test.c
./a.out
```

Suggestion: Add the following snippets to your `.bashrc`
```sh
# compile brainfuck to c and then to binary
bfC()
{
    ./hsfuck c $1 /tmp/ccode.c
    gcc /tmp/ccode.c -o $2
}
```

```sh
# simulate as MIPS (using SPIM)
bfMIPS()
{
    ./hsfuck mips $1 /tmp/mipscode.mips
    spim -file /tmp/mipscode.mips
}
```

## Commands
* `+` increment the value of the current cell
* `-` decrement the value of the current cell
* `»` right shift the value of the current cell
* `«` left shift the value of the current cell
* `>` move the tape one cell to the right
* `<` move the tape one cell to the left
* `.` print the value of the current cell as ASCII
* `,` read the value of an ASCII character from stdin to the current cell
* `:` print the value of the current cell as an integer
* `;` read an integer from stdin to the current cell
* `[c]` execute `c` while the value of the cell is not zero
* `#` print debug information

## References
* Brainfuck
    * [An introduction to programming in BF](https://www.iwriteiam.nl/Ha_bf_intro.html)
    * [BF is Turing-complete](https://www.iwriteiam.nl/Ha_bf_Turing.html)
* Optimizations
    * [Brainfuck optimization strategies](http://calmerthanyouare.org/2015/01/07/optimizing-brainfuck.html)
    * [BFC Optimizations](https://bfc.wilfred.me.uk/docs/optimisations)
    * [An Optimising BF Compiler](https://www.wilfred.me.uk/blog/2015/08/29/an-optimising-bf-compiler/)
    * [Even More BF Optimisations](https://www.wilfred.me.uk/blog/2015/10/18/even-more-bf-optimisations/)

## TO DO:
- [X] `0` set the cell to 0
- [X] `»` and `«` -> right and left shifts
- [X] Add more print and read options (integer)
- [X] remove register
- [X] compile to MIPS
- [X] Add debug to MIPS target
- [ ] Test MIPS and C output
- [X] Add compilation target flag
- [X] Add commands documentation
- [X] Add references
