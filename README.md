# hsfuck
![Tests CI](https://github.com/tttardigrado/hsfuck/actions/workflows/tests.yml/badge.svg)
![Build CI](https://github.com/tttardigrado/hsfuck/actions/workflows/haskell.yml/badge.svg)
![License](https://img.shields.io/github/license/tttardigrado/hsfuck)
<a href="https://twitter.com/intent/tweet?text=Check%20out%20hsfuck%20by%20%40_tardigrado_%20https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fhsfuck 😁"><img src="https://img.shields.io/twitter/url?style=social&url=https%3A%2F%2Fgithub.com%2Ftttardigrado%2Fhsfuck"></a>

![Logo](./Logo.png)

A _brainfuck_ to _C_ compiler written in _Haskell_

## Tech stack
* Languages: Haskell
* Packages: Parsec

## Blog Post
I wrote a [blog post](https://tttardigrado.github.io/posts/hsfuck/) about this project

## How to install and use
You need to have cabal and Haskell installed. Then run the following commands

```sh
# clone the repo and move to it
git clone https://github.com/tttardigrado/hsfuck
cd hsfuck

# build the project using cabal
cabal build

# optionally move the binary into another location
cp ./dist-newstyle/.../bf2 .

# run the compiler
# (fst argument is the path of the src file)
# (snd argument is the path of the output file) 
./hsfuck test.bf test.c

# compile and run the C code
gcc test.c
./a.out
```

Suggestion: Add this snippet to your `.bashrc`
```sh
bf()
{
    ./hsfuck $1 /tmp/ccode.c
    gcc /tmp/ccode.c -o $2
}
```

## TO DO:
- [X] `0` set the cell to 0
- [X] Register
- [X] `»` and `«` -> right and left shifts
- [X] `*`, `/`, `_`, `$` -> multiplication, int division, subtraction and sum of the current cell and the register
- [ ] `~` -> swap register and cell
- [X] `&` -> set register to cell
- [X] `$` -> set cell to register
- [ ] Add tests for `&` and `$`
- [ ] Add tests for `*`, `/`, `_`, `@`
- [ ] Add commands documentation