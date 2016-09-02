namespace Freya.Types

open FParsec

(* Types *)

type Parse<'a> =
    Parser<'a,unit>