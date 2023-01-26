module Program

open Koltrast.Frontend.Parser
open Koltrast.Frontend.Environment

match parseFile @"C:\Users\vince\RiderProjects\Koltrast\Koltrast\input.txt" with
| Ok ast -> ast |> createEnv |> printfn "%A"
| Error err -> printfn "%s" err |> exit 1
