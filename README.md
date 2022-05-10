# Nomad Calculator
! Continued version at https://github.com/dhansmair/nomad. Keeping this repository to keep links alive.


Haskell project for the course "Fortgeschrittene Funktionale Programmierung" at LMU. 
The Nomad calculator is a simple CLI tool to evaluate mathematical expressions that also supports defining functions. Nomad also supports lambda expressions, higher order functions and partial function application. A type check ensures the semantic correctness of user input.

![screenshot of the CLI](https://github.com/dhansmair/nomad-calculator/blob/main/docs/images/nomad_screenshot.png?raw=true)


## Setup
```
stack build                 // builds all executables
stack run cli               // runs cli executable
```


## Syntax
```
x = expr                    // define global variable
f(x,y) = expr               // define function
f = \x,y -> expr            // equivalently, functions can be defined as abstractions
expr                        // evaluate expressions
(\x -> 2*x)(4)              // lambda application
f(1,2)                      // function application
```

expressions can be:
``` 
1                           // any number
x                           // a variable
1 + 2 / 3 etc               // binary operations
f(x)                        // function call
(\x,y->x*y)(5,3)            // application of an abstraction
```

## Commands
```
:q                  close the cli
:t <expr>           show the type of an expression
:env                show the current environment (stored functions and variables)
```
