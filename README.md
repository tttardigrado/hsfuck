# hsfuck
**Build you a brainfuck compiler for the greater good**

A _brainfuck_ to _C_ compiler written in _Haskell_*

## Teck stack
Languages: Haskell
Packages: Parsec

## Blog Post
I wrote a [blog post](./blog/) about this project

## How to install and use
You need to have cabal and Haskell installed.

```sh
# clone the repo and move to it
git clone .....
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