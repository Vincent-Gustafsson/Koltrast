module Koltrast.ResultBuilder

open System

let ofOption error = function Some s -> Ok s | None -> Error error

type ResultBuilder() =
    member __.Return(x) = Ok x

    member __.ReturnFrom(m: 'T option) = m

    member __.Bind(m, f) = Result.bind f m

    member __.Zero() = None

    member __.Combine(m, f) = Option.bind f m

    member __.Delay(f: unit -> _) = f

    member __.Run(f) = f()

    member __.TryWith(m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally(m, compensation) =
        try __.ReturnFrom(m)
        finally compensation()

    member __.Using(res:#IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While(guard, f) =
        if not (guard()) then Some () else
        do f() |> ignore
        __.While(guard, f)

    member __.For(sequence:seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = new ResultBuilder()