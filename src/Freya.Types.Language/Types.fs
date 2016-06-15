namespace Freya.Types.Language

open System
open Freya.Types
open FParsec

(* RFC 5646

   Types, parsers and formatters implemented to mirror the specification of 
   Language Tag semantics as defined in RFC 5646.

   Taken from [http://tools.ietf.org/html/rfc5646] *)

(* Note: The current implementation does not implement either private use
   tags or grandfathered tags. Contributions are welcome if this is a real
   world issue for anyone.

   In addition, the main implementation of a language tag does not implement
   the "extension" property, or the optional "privateuse" property. As the RFC,
   even in the list of example language tags, never produces an example of either
   of these in use they have been assumed to be of low importance for now.

   However, if someone does show a valid and even slightly common use case,
   they will be implemented. *)

(* Language *)

type Language =
    | Language of string * string list option

    static member Mapping =

        let extP =
            skipChar '-' >>. Grammar.alpha 3 3

        let extLangP =
            choice [
                attempt (tuple3 extP extP extP) |>> fun (a, b, c) -> a :: b :: [ c ]
                attempt (tuple2 extP extP) |>> fun (a, b) -> a :: [ b ]
                extP |>> fun a -> [ a ] ]

        let languageP =
            choice [
                Grammar.alpha 2 3 .>>. opt (attempt extLangP) |>> Language
                Grammar.alpha 4 4 |>> (fun x -> Language (x, None))
                Grammar.alpha 5 8 |>> (fun x -> Language (x, None)) ]

        let extF =
            function | x -> Formatting.append "-" >> Formatting.append x

        let extLangF =
            function | xs -> Formatting.join extF id xs

        let languageF =
            function | Language (x, Some e) -> Formatting.append x >> extLangF e
                     | Language (x, _) -> Formatting.append x 

        { Parse = languageP
          Format = languageF }

    static member format =
        Mapping.format Language.Mapping

    static member parse =
        Mapping.parse Language.Mapping

    static member tryParse =
        Mapping.tryParse Language.Mapping

    override x.ToString () =
        Language.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        Language.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        Language.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        Language.tryParse

(* Language Tag *)

type LanguageTag =
    | LanguageTag of Language * Script option * Region option * Variant

    static member Mapping =

        let languageTagP =
            tuple4 Language.Mapping.Parse 
                   (opt (attempt Script.Mapping.Parse))
                   (opt (attempt Region.Mapping.Parse))
                   (Variant.Mapping.Parse)
            |>> fun (language, script, region, variant) ->
                LanguageTag (language, script, region, variant)

        let languageTagF =
            function | LanguageTag (language, script, region, variant) ->
                         let formatters =
                            [ Language.Mapping.Format language
                              (function | Some x -> Script.Mapping.Format x | _ -> id) script
                              (function | Some x -> Region.Mapping.Format x | _ -> id) region
                              Variant.Mapping.Format variant ]

                         fun b -> List.fold (|>) b formatters

        { Parse = languageTagP
          Format = languageTagF }

    static member format =
        Mapping.format LanguageTag.Mapping

    static member parse =
        Mapping.parse LanguageTag.Mapping

    static member tryParse =
        Mapping.tryParse LanguageTag.Mapping

    override x.ToString () =
        LanguageTag.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        LanguageTag.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        LanguageTag.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        LanguageTag.tryParse

(* Script *)

 and Script =
    | Script of string

    static member Mapping =

        let scriptP =
            skipChar '-' >>. Grammar.alpha 4 4 |>> Script

        let scriptF =
            function | Script x -> Formatting.append "-" >> Formatting.append x

        { Parse = scriptP
          Format = scriptF }

(* Region *)

 and Region =
    | Region of string

    static member Mapping =

        let regionP =
            skipChar '-' >>. (Grammar.alpha 2 2 <|> Grammar.digit 3 3) |>> Region

        let regionF =
            function | Region x -> Formatting.append "-" >> Formatting.append x

        { Parse = regionP
          Format = regionF }

(* Variant *)

 and Variant =
    | Variant of string list

    static member Mapping =

        let alphaPrefixVariantP =
            Grammar.alphaNum 5 8

        let digitPrefixVariantP =
            satisfy isDigit .>>. Grammar.alphaNum 3 3 |>> fun (c, s) -> sprintf "%c%s" c s

        let varP =
            skipChar '-' >>. (alphaPrefixVariantP <|> digitPrefixVariantP)

        let variantP =
            many varP |>> Variant

        let varF =
            function | x -> Formatting.append "-" >> Formatting.append x

        let variantF =
            function | Variant xs -> Formatting.join varF id xs

        { Parse = variantP
          Format = variantF }

(* RFC 4647

   Types, parsers and formatters implemented to mirror the specification of 
   Language Range semantics as defined in RFC 4647.

   Taken from [http://tools.ietf.org/html/rfc4647] *)

type LanguageRange =
    | Range of string list
    | Any

    static member Mapping =

        let languageRangeP =
            choice [
                skipChar '*' >>% Any
                Grammar.alpha 1 8 .>>. many (skipChar '-' >>. Grammar.alphaNum 1 8) |>> (fun (x, xs) -> Range (x :: xs)) ]


        let languageRangeF =
            function | Range x -> Formatting.join Formatting.append (Formatting.append "-") x
                     | Any -> Formatting.append "*"

        { Parse = languageRangeP
          Format = languageRangeF }

    static member format =
        Mapping.format LanguageRange.Mapping

    static member parse =
        Mapping.parse LanguageRange.Mapping

    static member tryParse =
        Mapping.tryParse LanguageRange.Mapping

    override x.ToString () =
        LanguageRange.format x

    (* Obsolete

       To be removed in 4.0. *)

    [<Obsolete ("Use format instead.")>]
    static member Format =
        LanguageRange.format

    [<Obsolete ("Use parse instead.")>]
    static member Parse =
        LanguageRange.parse

    [<Obsolete ("Use tryParse instead.")>]
    static member TryParse =
        LanguageRange.tryParse