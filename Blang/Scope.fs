module Blang.Scope

open Blang.RuntimeTypes

/// Looks up a value by the given name within the given scope.
/// If that name doesn't exist, recursively searches the enclosing scope.
/// Returns Some value if found, else None.
let rec lookupValue (name: string) (scope: Scope) : Value option =
    match scope.SymbolTable.TryGetValue name with
    | true, v -> Some v
    | _ -> scope.EnclosingScope |> Option.bind (lookupValue name)

let create (parent: Scope option) =
    { EnclosingScope = parent
      SymbolTable = Map.empty }

let bindValueTo (name: string) (value: Value) (scope: Scope) : Scope =
    { scope with SymbolTable = scope.SymbolTable |> Map.add name value }