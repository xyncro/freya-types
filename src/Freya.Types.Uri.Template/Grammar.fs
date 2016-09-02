namespace Freya.Types.Uri.Template

open Freya.Types

(* RFC 6570

   Types, parsers and formatters implemented to mirror the specification of 
   URI Template semantics as defined in RFC 6570.

   Taken from [http://tools.ietf.org/html/rfc6570] *)

(* Grammar

   NOTE: We do not currently support IRIs - this may
   be supported in future. *)

[<RequireQualifiedAccess>]
module Grammar =

    let isLiteral i =
            i = 0x21
         || i >= 0x23 && i <= 0x24
         || i = 0x26
         || i >= 0x28 && i <= 0x3b
         || i = 0x3d
         || i >= 0x3f && i <= 0x5b
         || i = 0x5d
         || i = 0x5f
         || i >= 0x61 && i <= 0x7a
         || i = 0x7e

    let isVarchar i =
            Grammar.isAlpha i
         || Grammar.isDigit i
         || i = 0x5f // _
