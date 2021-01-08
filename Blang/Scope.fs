module Blang.Scope

/// Looks up a value by the given name within the given scope.
/// If that name doesn't exist, recursively searches the enclosing scope.
/// Returns Some value if found, else None.
let rec lookupValue (name: string) (scope: RuntimeTypes.Scope) : RuntimeTypes.Value option =
    match scope.SymbolTable.TryGetValue name with
    | true, v -> Some v
    | _ -> scope.EnclosingScope |> Option.bind (lookupValue name)

let create (parent: RuntimeTypes.Scope option) =
    { RuntimeTypes.Scope.EnclosingScope = parent
      RuntimeTypes.Scope.SymbolTable = Map.empty }