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
            | Success (x, _, _) -> Choice1Of2 x
            | Failure (e, _, _) -> Choice2Of2 e

    let parse (mapping: Mapping<'a>) =
        fun s ->
            match tryParse mapping s with
            | Choice1Of2 x -> x
            | Choice2Of2 e -> failwith e