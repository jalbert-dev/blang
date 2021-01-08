module Blang.Runtime

let ( <!> ) a b = Result.map b a

let evaluate scope value = 
    Evaluator.evaluate RuntimeCore.functionMap scope value
    <!> fun (result, sideEffects) -> 
            result, Evaluator.applySideEffects scope sideEffects