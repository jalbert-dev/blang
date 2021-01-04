module Blang.RuntimeTypes

type LineInfo =
    { Line: int
      Character: int }

// Expressions work kinda like informal s-expressions.
// Atom values evaluate immediately to their value.
// Expression values are composed of any number of other values.
//   When evaluated, the first value in the expression is evaluated and
//   the resulting symbol used is interpreted as the name of a function
//   bound in the evaluation scope.
//   The remaining values are evaluated. Any remaining symbol values are
//   interpreted as identifiers of values bound to the current scope and replaced
//   with those values. Then the resulting list of values is passed to
//   the function being invoked.
//   If the expression contains no values, it evaluates to itself.
// Note that Expression values are only required to have a leading symbol value
// when they are evaluated. This means Expression values, like s-expressions,
// are usable as lists and can store arbitrary values for as long as they
// remain unevaluated.

type ValueType =
    | NumberAtom of double
    | SymbolAtom of string
    | StringAtom of string
    | Expression of Value list
and Value =
    { Type: ValueType
      /// The line position that this value was created from, if the
      /// value was created from a token or tokens. If this value was
      /// created from evaluation (for example 'a + b'),
      /// then that value is "anonymous" and has a position of None.
      Position: LineInfo option }

// Functions are currently not a distinct case of a bound value.
// They can be represented as a bound (and thus unevaluated) 
// expression of form (args body), for example. Or in script:
//     (' (x y) (+ x y))

type Scope =
      /// The parent scope of this Scope. Used during value lookup and error tracing.
      ///
      /// At the moment I'm only concerned with walking backwards up the chain of parents,
      /// so to keep things simple parents aren't aware of their child scopes.
    { EnclosingScope: Scope option
      /// A Scope's symbol table names values accessible within that scope.
      /// Failed lookups in most cases should fall back to the enclosing scope.
      SymbolTable: Map<string, Value> }
