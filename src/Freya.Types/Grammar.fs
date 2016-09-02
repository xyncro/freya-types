namespace Freya.Types

(* Grammar *)

[<RequireQualifiedAccess>]
module Grammar =

    (* RFC 5234

       Core ABNF grammar rules as defined in RFC 5234, expressed
       as predicates over integer character codes.

       Taken from RFC 5234, Appendix B.1 Core Rules
       See [http://tools.ietf.org/html/rfc5234#appendix-B.1] *)

    let isAlpha i =
            i >= 0x41 && i <= 0x5a
         || i >= 0x61 && i <= 0x7a

    let isDigit i =
            i >= 0x30 && i <= 0x39

    let isDquote i =
            i = 0x22

    let isHexdig i =
            isDigit i
         || i >= 0x41 && i <= 0x46
         || i >= 0x61 && i <= 0x66

    let isHtab i =
            i = 0x09

    let isSp i =
            i = 0x20

    let isVchar i =
            i >= 0x21 && i <= 0x7e

    let isWsp i =
            isHtab i
         || isSp i