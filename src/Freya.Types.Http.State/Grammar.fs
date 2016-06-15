namespace Freya.Types.Http.State

open Freya.Types
open FParsec

(* RFC 6265

   Types, parsers and formatters implemented to mirror the specification of
   HTTP State Management Mechanism semantics (commonly known as cookies) as
   defined in RFC 6265.

   Taken from [http://tools.ietf.org/html/rfc6265] *)

(* Grammar *)

[<RequireQualifiedAccess>]
module Grammar =

    (* Parsers *)

    let sp : Parse<unit> =
        skipSatisfy (int >> Grammar.isSp)