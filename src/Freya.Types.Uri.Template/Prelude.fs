namespace Freya.Types.Uri.Template

open FParsec

(* Parsers

   Some extra functions for parsing, in particular for dynamically
   parsing using a list of dynamically constructed parsers which should
   succeed or fail as a single parser. *)

[<AutoOpen>]
module private Parsers =

    let multi parsers =
        fun stream ->
            let rec eval state =
                match state with
                | vs, [] ->
                    Reply (vs)
                | vs, p :: ps ->
                    match p stream with
                    | (x: Reply<'a>) when x.Status = Ok -> eval (x.Result :: vs, ps)
                    | (x) -> Reply<'a list> (Status = x.Status, Error = x.Error)

            eval ([], parsers)

    let multiSepBy parsers sep =
        fun stream ->
            let rec eval state =
                match state with
                | _, vs, [] ->
                    Reply (vs)
                | true, vs, ps ->
                    match sep stream with
                    | (x: Reply<unit>) when x.Status = Ok -> eval (false, vs, ps)
                    | (x) -> Reply<'a list> (Status = x.Status, Error = x.Error)
                | false, vs, p :: ps ->
                    match p stream with
                    | (x: Reply<'a>) when x.Status = Ok -> eval (true, x.Result :: vs, ps)
                    | (x) -> Reply<'a list> (Status = x.Status, Error = x.Error)

            eval (false, [], parsers)
