namespace Freya.Types.Uri

open System
open System.Text
open Freya.Types
open FParsec

(* Prelude

   Common functions for dealing with URI data, dealing with percentage encoding
   and with parsing of common IP Address forms. *)

(* Encoding *)

[<RequireQualifiedAccess>]
module Encoding =

    (* UTF-8

       Shorthand for UTF-8 encoding and decoding of strings (given the
       assumption that the .NET UTF-16/Unicode string is our basic string
       type). *)

    [<RequireQualifiedAccess>]
    module private UTF8 =

        let encode : string -> byte list =
            Encoding.UTF8.GetBytes >> List.ofArray

        let decode : byte list -> string =
            List.toArray >> fun x -> Encoding.UTF8.GetString (x, 0, x.Length)

    (* Percent-Encoding

       Code for percent-encoding data given some simple assumptions about what
       should be allowed through unencoded. *)

    [<RequireQualifiedAccess>]
    module Percent =

        (* Grammar *)

        let private pct =
            byte 0x25

        (* Indices

            Simple lookups/indices for converting between bytes and the hex
            encoding of those bytes. *)

        let private toBytePair (s: string) =
            byte >> fun i -> i, UTF8.encode (i.ToString s)

        let private index =
            let range = [ 0x00 .. 0xff ]
            let lower = List.map (toBytePair "x2") range
            let upper = List.map (toBytePair "X2") range
        
            lower @ upper

        let private byteIndex =
            index
            |> Map.ofList

        let private hexIndex =
            index
            |> List.map (fun (a, b) -> (b, a))
            |> Map.ofList

        (* Parsing

            Parsing functions, providing a function to create a parser
            given a whitelist of allowed characters within the input (pct-encoded
            values are implicitly allowed, and remain unaltered by the parser. *)

        let private pctP : Parse<char []>=
            tuple3 (pchar '%') hex hex 
            |>> (fun (p, a, b) ->
                [| p; a; b |])

        let parser (pred: int -> bool) =
            many (attempt pctP <|> (satisfy (int >> pred) |>> fun x -> [| x |]))
            |>> fun x ->
                new string (Array.concat x)

        (* Formatting

            Formatting functions, providing a function to create an formatter
            for a given string. No encoding is done as part of formatting, any
            characters within the provided string are assumed to be valid. *)

        let formatter () =
            Formatting.append

        (* Encoding

           Encoding functions, providing a function to create an encoder for
           a string given a whitelist of allowed characters within the input
           (non-whitelisted characters are pct-encoded automatically). *)

        let private hexdig =
            int >> Grammar.isHexdig

        let private format p =
            let rec format r =
                function | [] -> r
                            | h :: x :: y :: t when h = pct && hexdig x && hexdig y -> format (r @ [ h; x; y ]) t
                            | h :: t when p (int h) -> format (r @ [ h ]) t
                            | h :: t -> format (r @ [ pct ] @ Map.find h byteIndex) t

            format []

        let encoder (pred: int -> bool) =
            UTF8.encode >> format pred >> UTF8.decode

        (* Decoding

           Decoding functions, providing a function to create a decoder for a
           pct-encoded string, converting pct-encoded values to their Unicode/UTF-16
           form. *)

        let private pctDecodeP : Parser<char,unit> =
            pchar '%' >>. hex .>>. hex
            |>> fun (a, b) ->
                char (Map.find ([ byte a; byte b ]) hexIndex)

        let private decodeP =
            many (attempt pctDecodeP <|> anyChar)
            |>> fun x ->
                new string (Array.ofList x)

        let decoder () =
            fun s ->
                match run decodeP s with
                | Success (s, _, _) -> s
                | _ -> failwith "Decode Failure"

(* IP Address *)

[<RequireQualifiedAccess>]
module IPAddress =

    [<RequireQualifiedAccess>]
    module V4 =

        let private isv4Char i =
                Grammar.isDigit i
             || i = 0x2e // .

        let format x =
            Formatting.append x

        let parse : Parse<string> =
            many1Satisfy (int >> isv4Char) >>= (fun x ->
                match Uri.CheckHostName x with
                | UriHostNameType.IPv4 -> preturn x
                | _ -> pzero)

    [<RequireQualifiedAccess>]
    module V6 =

        let private isv6Char i =
                Grammar.isHexdig i
             || i = 0x3a // :

        let format x =
            Formatting.append "[" >> Formatting.append x >> Formatting.append "]"

        let parse : Parse<string> =
            skipChar '[' >>. (many1Satisfy (int >> isv6Char) >>= (fun x ->
                match Uri.CheckHostName x with
                | UriHostNameType.IPv6 -> preturn x
                | _ -> pzero)) .>> skipChar ']'