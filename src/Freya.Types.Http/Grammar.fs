namespace Freya.Types.Http

open Freya.Types
open FParsec

(* RFC 7230

   Types, parsers and formatters implemented to mirror the specification of 
   HTTP semantics as defined in RFC 7230.

   Taken from [http://tools.ietf.org/html/rfc7230] *)

(* Grammar *)

[<RequireQualifiedAccess>]
module Grammar =

    (* Field Value Components

       Taken from RFC 7230, Section 3.2.6. Field Value Components
       See [http://tools.ietf.org/html/rfc7230#section-3.2.6] *)

    let isTchar i =
            Grammar.isAlpha i
         || Grammar.isDigit i
         || i = 0x21 // !
         || i >= 0x23 && i <= 0x26 // # $ % &
         || i = 0x5c // \
         || i = 0x2a // *
         || i = 0x2b // +
         || i = 0x2d // -
         || i = 0x2e // .
         || i = 0x5e // ^
         || i = 0x5f // _
         || i = 0x60 // `
         || i = 0x7c // |
         || i = 0x7e // ~

    let isObstext i =
            i >= 0x80 && i <= 0xff

    let isQdtext i =
            Grammar.isHtab i
         || Grammar.isSp i
         || i = 0x21
         || i >= 0x23 && i <= 0x5b
         || i >= 0x5d && i <= 0x7e
         || isObstext i

    let isQuotedPairChar i =
            Grammar.isHtab i
         || Grammar.isSp i
         || Grammar.isVchar i
         || isObstext i

    (* Parsers *)

    (* Whitespace

        Taken from RFC 7230, Section 3.2.3. Whitespace
        See [http://tools.ietf.org/html/rfc7230#section-3.2.3] *)

    let ows : Parse<unit> =
        skipManySatisfy (int >> Grammar.isWsp)

    let token : Parse<string> = 
        many1Satisfy (int >> isTchar)

    let quotedPair : Parser<char, unit> =
            skipChar '\\' 
        >>. satisfy (int >> isQuotedPairChar)

    let quotedString : Parser<string, unit> =
            skipSatisfy (int >> Grammar.isDquote)
        >>. many (quotedPair <|> satisfy (int >> isQdtext)) |>> (List.toArray >> System.String >> string)
        .>> skipSatisfy (int >> Grammar.isDquote)

    (* ABNF List Extension: #rule

        Taken from RFC 7230, Section 7. ABNF List Extension: #rule
        [http://tools.ietf.org/html/rfc7230#section-7] *)

    let infixHead p s =
        (attempt p |>> Some) <|> (s >>% None)

    let infixTail p s =
        many (ows >>? s >>? ows >>? opt p)

    (* Note:
        The infix and prefix parsers are designed to convey as accurately as possible the 
        meaning of the ABNF #rule extension including the laxity of specification for backward 
        compatibility. Whether they are a perfectly true representation is open to debate, 
        but they should perform sensibly under normal conditions. *)

    let infix p s = 
        infixHead p s .>>. infixTail p s .>> ows |>> fun (x, xs) -> x :: xs |> List.choose id

    let infix1 p s =
        notEmpty (infix p s)

    let prefix p s =
        many (ows >>? s >>? ows >>? p)


