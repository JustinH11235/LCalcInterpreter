# LCalcInterpreter
An interpreter for the Lambda Calculus language written in Haskell.

## How to Run
1. Clone this repository onto your local machine
2. In your terminal, navigate into the new directory
3. Compile the source yourself using GHC: `ghc LCalcInterpreter.hs`
4. Now run `./LCalcInterpreter {your_filename}` (ex. `./LCalcInterpreter example.lc` to run example program)

## Syntax Highlighting
If you use Visual Studio Code as your text editor, there is currently no extension on the marketplace that adds syntax highlighting for my language, however, I made a syntax highlighter that you can use (https://github.com/JustinH11235/lcalc-syntax-highlighting) by simply placing this repository's folder in your `<user home>/.vscode/extensions` folder.\
I am planning to add this extension to the VSCode marketplace in the future.

## What is Lambda Calculus?
Lambda Calculus is a Turing complete system that uses only functions for computing and is made up of only 3 stuctures:
* A way to define functions (ex. '\a. a' is the identity function that takes in an input 'a' and evaluates to that input)
* A way to refer to functions (ex. the variable 'a')
* A way to apply functions to each other (ex. 'a b' means a applied to b)
Lambda Calculus is the idea at the root of functional programming languages (including Haskell which I programmed this in).

## Lexing
I opted to mix the lexing into my parsing, meaning instead of separately creating an array of tokens, I parse the input string directly, which seemed more suitable for my choice of parser.

## Parsing
I implemented a type of parser called a parser combinator, which is where you create very small parsers and combine them together to create bigger and more useful ones.

## Evaluation
Evaluation for Lambda Calculus is relatively simple, all you do is evaluate function applications by replacing the free variables with their input.

## Plans
- [x] Parse an input file as input instead of through stdin. This way you can create a .lc file and then use GHC to transpile your lambda calculus code into Haskell.
- [x] Use De Bruijn indexing (https://en.wikipedia.org/wiki/De_Bruijn_index) to allow variable names to be reused (at the moment you need to be careful to not let variables shadow each other).
- [x] Create a standard library of functions like ADD, MULT, etc. and allow for substitution
- [x] Allow users to write int literals such as 6 and create aliases for the Church Numeral representations of these
- [x] Add to the syntax of the language by allowing assignment, such as A = (\a. a), so that A can be reused later in the code as an alias for (\a. a).
- [x] After converting the De Bruijn abstract syntax tree into normal variable names, make sure there is no incorrect shadowing that occurs (i.e. make the converted tree a valid equivalent)
