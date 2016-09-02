namespace Freya.Types.Http

open System

(* Prelude *)

(* String Extensions *)

[<RequireQualifiedAccess>]
module internal String =

    let equalsCI s1 s2 =
        String.Equals (s1, s2, StringComparison.OrdinalIgnoreCase)

(* List Extensions *)

[<RequireQualifiedAccess>]
module internal List =

    let chooseMaxBy projection =
            List.map (fun x -> x, projection x)
         >> List.choose (function | (x, Some y) -> Some (x, y) | _ -> None)
         >> List.sortBy (fun (_, y) -> y)
         >> List.map fst
         >> function | [] -> None
                     | x :: _ -> Some x