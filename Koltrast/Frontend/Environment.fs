module Koltrast.Frontend.Environment

open Koltrast.Frontend.AST

type VarSymbol = | VarSymbol of option<Type> * Mutability

type Env = {
    varDefinitions: Map<string, VarSymbol>
    // funDefinitions: Map<string, FunSymbol>
}

module Env =
    let empty = { varDefinitions=Map.empty }
    
    let addVar 


usually the environment is just a immutable map name -> type
Confusedswede [Koltrast] — Today at 5:47 PM
How does that work with scoping? I'm used to having a stack of maps
πρ — Today at 5:47 PM
and the trick is that you have to thread it through correctly in your inference functions
exactly so
imagine you're typing something that looks like let x = y in z


you start with an initial / empty env, and that gets augmented as its threaded through your functions
so when you process something like let x = y in z, you're starting with some initial env
then your env gets augmented with x=y and becomes env'
then z is processed using env'
then outside this binding, things are processed with the old env
and since env is immutable, you don't have to worry about unpopping anything
so your context is flat, gets augmented with new bindings
and persistent, so you don't have to unaugment