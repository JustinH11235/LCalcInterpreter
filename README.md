# LCalcInterpreter
An interpreter for my Lambda Calculus based language LCalc, written in Haskell.

![Screenshot ](/images/lcalc-syntax-highlighting.png?raw=true)

## **Overview**
I had 3 goals with this project:
1. **To learn how programming language interpreters are made.**

These are the programs that "interpret" code such as a Python file as code that your computer can directly run. When someone says they "made a programming language," at the end of the day that means they programmed a interpreter (or compiler) that translates a string of text (a plaintext file) into another, already invented programming language

e.g. Python is an interpreter that interprets a text file written in Python syntax, and runs the equivalent C code.

2. **To dive deeper into Lambda Calculus.**

I talk more about this below, but Lambda Calculus is a really interesting, simplistic model for computing, which I like to simplify as: everything is a function. Lambda Calculus is so cool because it can compute anything a [Turing Machine](https://www.cl.cam.ac.uk/projects/raspberrypi/tutorials/turing-machine/one.html) can compute (i.e. it is Turing Complete, just like your laptop's CPU), yet it's made up of very few parts.

3. **To learn a functional programming language.**

In school you typically focus on learning imperative languages such as Java, C, and C++  as opposed to functional languages because imperative languages are the most popular. However, I had heard that functional languages provide a unique way of programming so I looked for the best one to learn and I found Haskell.

---

This project is the combination of these 3 goals. In this repository I use Haskell to code a programming language interpreter from scratch, without the use of external parsing libraries.

The language that I am interpreting is a language I designed, but is strongly based on Lambda Calculus with some added features to make it more useable as a real programming language.

These "added features" consist of two main additions: a standard library of built-in functions that you can use such as adding, dividing, creating lists, and performing recursion, as well as the ability to easily interpret certain data types like numbers and booleans as literal values (i.e. 0, 1, 2, True, False) rather than their function representations.

This project is fully functional, in more ways than one, as it can be used following the `How to Run` instructions below, is programmed in Haskell (a purely functional language), and implements Lambda Calculus, the basis of all functional languages.

**I didn't make this project with any real intended use case in mind, it was more to advance my learning, but after finishing it, I honestly believe it might be one of the best educational tools to learn Lambda Calculus out there. If you want to try to come up with your own Lambda Calculus functions, you can use LCalc to see if they work as intended by running them directly as actual code.**

## How to Run
1. Clone this repository onto your local machine
2. In your terminal, navigate into the new directory
3. Compile the source yourself using [GHC](https://www.tutorialspoint.com/haskell/haskell_environment_setup.htm): `ghc LCalcInterpreter.hs`
4. Now run `./LCalcInterpreter {your_filename}` (ex. `./LCalcInterpreter example.lc` to run example program)

## Syntax Highlighting
I made my own syntax highlighter [extension](https://marketplace.visualstudio.com/items?itemName=justinh0.lcalc-syntax-highlighting) for Visual Studio Code that you can install in the normal VSCode Extension Marketplace.
If you want to see the source code, here is the [repository](https://github.com/JustinH11235/lcalc-syntax-highlighting).

## What is Lambda Calculus?
[Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is a Turing complete system that uses only functions for computing and is made up of only 3 stuctures:
* A way to define functions (ex. `(\a. a)` is the identity function that takes in an input 'a' and evaluates to that input)
* A way to refer to functions (ex. the variable `a`)
* A way to apply functions to each other (ex. `a b` means a applied to b)

Lambda Calculus is the idea at the root of functional programming languages (including Haskell which I programmed this in).

If you want to learn more, here is the [Numberphile video](https://www.youtube.com/watch?v=eis11j_iGMs) that inspired me to learn about this topic.

## Lexing
I opted to mix the lexing into my parsing, meaning instead of separately creating an array of tokens, I parse the input string directly, which seemed more suitable for my choice of parser.

## Parsing
I implemented a type of parser called a parser combinator, which is where you create very small parsers and combine them together to create bigger and more useful ones.

## Evaluation
Evaluation for Lambda Calculus is relatively simple, all you do is evaluate function applications by replacing the free variables with their input.

## Plans
- [x] Parse an input file as input instead of through stdin. This way you can create a .lc file and then use GHC to transpile your lambda calculus code into Haskell.
- [x] Use [De Bruijn indexing](https://en.wikipedia.org/wiki/De_Bruijn_index) to allow variable names to be reused (at the moment you need to be careful to not let variables shadow each other).
- [x] Create a standard library of functions like ADD, MULT, etc. and allow for substitution
- [x] Allow users to write int literals such as 6 and create aliases for the Church Numeral representations of these
- [x] Add to the syntax of the language by allowing assignment, such as A = (\a. a), so that A can be reused later in the code as an alias for (\a. a).
- [x] After converting the De Bruijn abstract syntax tree into normal variable names, make sure there is no incorrect shadowing that occurs (i.e. make the converted tree a valid equivalent)
