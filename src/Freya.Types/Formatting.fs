namespace Freya.Types

open System.Globalization
open System.Text

(* Types *)

type Format<'a> =
    'a -> StringBuilder -> StringBuilder

(* Formatting *)

[<RequireQualifiedAccess>]
module Formatting =

    let append (s: string) (b: StringBuilder) =
        b.Append (s)

    let appendf1 (s: string) (v1: obj) (b: StringBuilder) =
        b.AppendFormat (CultureInfo.InvariantCulture, s, v1)

    let appendf2 (s: string) (v1: obj) (v2: obj) (b: StringBuilder) =
        b.AppendFormat (CultureInfo.InvariantCulture, s, v1, v2)

    let join<'a> (f: Format<'a>) (s: StringBuilder -> StringBuilder) =
        let rec join values (b: StringBuilder) =
            match values with
            | [] -> b
            | [v] -> f v b
            | v :: vs -> (f v >> s >> join vs) b

        join