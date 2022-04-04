# hscalc
Calculator and Plotter in Haskell


## Setup
```
stack build                 // builds all executables
stack run cli               // runs cli executable
stack run gui               // runs gui executable

evtl erst stack init ..?
```


## Syntax
```
x = expr                    // define global variable
f(x,y) = expr               // define function
f = \x,y -> expr            // equivalently, functions can be defined with abstractions
expr                        // evaluate expressions
(\x -> 2*x)(4)              // lambda application
f(1,2)                      // function application
```

expressions can be:
``` 
1                           // any number
x                           // a variable
1 + 2 / 3 etc               // binary operations
f(x)                        // application of a supercombinator / "function call"
(\x,y->x*y)(5,3)            // application of an abstraction
```

## Commands
```
:q                  close the cli
:t <expr>           show the type of an expression
:env                show the current environment (stored functions and variables)
```


## Data Structure of the Syntax Tree

```
data Stmt = Def String Expr
          | Expr Expr

newtype Builtin = B ([Expr] -> Either String Expr)

data Expr = Num Double
          | Var String
          | BinOp Op Expr Expr
          | App Expr [Expr] 
          | Lambda [String] Expr
          | Builtin Builtin

data Op = Add | Sub | Mul | Div | Pow
```




### Links

http://mdaines.github.io/grammophone/#/
