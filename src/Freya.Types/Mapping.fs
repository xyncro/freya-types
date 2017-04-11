namespace Freya.Types

open System.Text
open FParsec

(* Types *)

type Mapping<'a> =
    { Parse: Parse<'a>
      Format: Format<'a> }

(* Mapping *)

[<RequireQualifiedAccess>]
module Mapping =

    let format (mapping: Mapping<'a>) =
        fun a ->
            string (mapping.Format a (StringBuilder ()))

    let tryParse (mapping: Mapping<'a>) =
        fun s ->
            match run (mapping.Parse .>> eof) s with
            | Success (x, _, _) -> Result.Ok x
            | Failure (e, _, _) -> Result.Error e

    let parse (mapping: Mapping<'a>) =
        fun s ->
            match tryParse mapping s with
            | Result.Ok x -> x
            | Result.Error e -> failwith e
