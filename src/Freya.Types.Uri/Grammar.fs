namespace Freya.Types.Uri

open Freya.Types

(* RFC 3986

   Types, parsers and formatters implemented to mirror the specification of 
   URI semantics as defined in RFC 3986.

   Taken from [http://tools.ietf.org/html/rfc3986] *)

(* Grammar *)

[<RequireQualifiedAccess>]
module Grammar =

    let isUnreserved i =
            Grammar.isAlpha i
         || Grammar.isDigit i
         || i = 0x2d // -
         || i = 0x2e // .
         || i = 0x5f // _
         || i = 0x7e // ~

    let isGenDelim i =
            i = 0x3a // :
         || i = 0x2f // /
         || i = 0x3f // ?
         || i = 0x23 // #
         || i = 0x5b // [
         || i = 0x5d // ]
         || i = 0x40 // @

    let isSubDelim i =
            i = 0x21 // !
         || i = 0x24 // $
         || i = 0x26 // &
         || i = 0x5c // \
         || i >= 0x28 && i <= 0x2c //   * + ,
         || i = 0x3b // ;
         || i = 0x3d // =

    let isReserved i=
            isGenDelim i
         || isSubDelim i