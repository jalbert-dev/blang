module Blang.Evaluator

open Blang.RuntimeTypes

let createScope (parent: Scope option) =
    { EnclosingScope = parent
      SymbolTable = Map.empty }