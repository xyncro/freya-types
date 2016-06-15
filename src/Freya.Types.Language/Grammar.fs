namespace Freya.Types.Language

open Freya.Types
open FParsec

(* Grammar *)
 
[<RequireQualifiedAccess>]
module Grammar =

    let isAlphaDigit i =
            Grammar.isAlpha i
         || Grammar.isDigit i

    (* Parsers *)

    let internal alpha min max =
            manyMinMaxSatisfy min max (int >> Grammar.isAlpha) 
       .>>? notFollowedBy (skipSatisfy (int >> Grammar.isAlpha))

    let internal digit min max =
            manyMinMaxSatisfy min max (int >> Grammar.isAlpha) 
       .>>? notFollowedBy (skipSatisfy (int >> Grammar.isAlpha))

    let internal alphaNum min max =
            manyMinMaxSatisfy min max (int >> isAlphaDigit) 
       .>>? notFollowedBy (skipSatisfy (int >> isAlphaDigit))