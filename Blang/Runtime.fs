module Blang.Runtime

let ( <!> ) a b = Result.map b a

/// <summary>Evaluates the given value inside the given scope.</summary>
/// <param name="scope">The scope to evaluate within.</param>
/// <param name="value">The value to be evaluated.</param>
/// <returns>A pair containing the value and updated scope resulting from 
/// evaluation, or an error type.</returns>
let evaluate scope value = 
    Evaluator.evaluateValue RuntimeCore.functionMap scope value
    <!> fun (result, sideEffects) -> 
            result, Evaluator.applySideEffects scope sideEffects