module Blang.RuntimeTypes

// Expressions work kinda like informal s-expressions.
// Atomic exprs evaluate immediately to their value.
// Expression exprs are composed of any number of other exprs.
//   When evaluated, the first expr in the expression expr is evaluated and
//   the resulting symbol used is interpreted as the name of a function
//   bound in the evaluation scope.
//   The remaining exprs are evaluated and the resulting values passed to
//   the function being invoked.
// Note that Expression exprs are only required to have a leading symbol value
// when they are evaluated. This means Expression exprs, like s-expressions,
// are usable as lists and can store arbitrary values for as long as they
// remain unevaluated.
type Expr =
    | NumberAtom of double
    | SymbolAtom of string
    | StringAtom of string
    | Expression of Expr list

// Functions are currently not a distinct case of a bound value.
// They can be represented as a bound (and thus unevaluated) 
// expression of form ((x y) (+ x y)), for example. Or in script:
//     '((x y) (+ x y))

type Scope =
    { EnclosingScope: Scope 
      SymbolTable: Map<string, Expr> }
