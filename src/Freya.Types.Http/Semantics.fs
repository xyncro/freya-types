namespace Freya.Types.Http

open Freya.Types.Language

(* Negotiation

   Forms of negotiation for various types, providing a negotiate function taking
   the supported options, and an optional list of suitable acceptable options,
   returning some list of available, sorted options when a negotiation has
   occurred, or none when it has not. *)

(* Charset *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Charset =

    let private max (Charset c) =
        function | AcceptableCharset (CharsetRange.Charset (Charset c'), _) when String.equalsCI c c' -> Some 0
                 | AcceptableCharset (CharsetRange.Any, _) -> Some 1
                 | _ -> None

    let private map requested =
        List.map (fun (x: Charset) ->
            x, List.chooseMaxBy (max x) requested)

    let private sort =
        List.sortBy (fun (_, y) ->
            (function | Some (AcceptableCharset (_, Some (Weight w))) -> 1. - w
                      | _ -> 0.) y)

    let private choose =
        List.choose (fun (x, y) ->
            (function | Some (AcceptableCharset (_, Some (Weight w))) when w > 0. -> Some x
                      | Some (AcceptableCharset (_, None)) -> Some x
                      | _ -> None) y)

    let private run requested =
            map requested
         >> sort
         >> choose

    let negotiate supported acceptable =
        run (Set.toList acceptable) (Set.toList supported)

(* ContentCoding *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module ContentCoding =

    // TODO: Better ContentCoding Negotiation - proper support of identity, etc.

    let private max (ContentCoding c) =
        function | AcceptableEncoding (EncodingRange.Coding (ContentCoding c'), _) when String.equalsCI c c' -> Some 0
                 | AcceptableEncoding (EncodingRange.Any, _) -> Some 1
                 | _ -> None

    let private map requested =
        List.map (fun (x: ContentCoding) ->
            x, List.chooseMaxBy (max x) requested)

    let private sort =
        List.sortBy (fun (_, y) ->
            (function | Some (AcceptableEncoding (_, Some (Weight w))) -> 1. - w
                      | _ -> 0.) y)

    let private choose =
        List.choose (fun (x, y) ->
            (function | Some (AcceptableEncoding (_, Some (Weight w))) when w > 0. -> Some x
                      | Some (AcceptableEncoding (_, None)) -> Some x
                      | _ -> None) y)

    let private run requested =
            map requested 
         >> sort
         >> choose

    let negotiate supported acceptable =
        run (Set.toList acceptable) (Set.toList supported)

(* Language *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Language =

    (* Note: This is intended to approximate the semantics
       of Basic Filtering as specified in Section 3.3.1 of RFC 4647.
       See [http://tools.ietf.org/html/rfc4647#section-3.3.1] *)

    (* Negotiation *)

    let private reify tag =
        let language, extensions =
            (function | LanguageTag (Language (l, Some e), _, _, _) -> [ l ], e
                      | LanguageTag (Language (l, _), _, _, _) -> [ l ], []) tag

        let script =
            (function | LanguageTag (_, Some (Script s), _, _) -> [ s ]
                      | _ -> []) tag

        let region =
            (function | LanguageTag (_, _, Some (Region r), _) -> [ r ]
                      | _ -> []) tag
        let variant =
            (function | LanguageTag (_, _, _, Variant variant) -> variant) tag

        List.concat [
            language
            extensions
            script
            region
            variant ]

    let private eq tag =
        Seq.zip (reify tag) >> Seq.forall ((<||) String.equalsCI)

    let private sort =
        List.sortBy (function | AcceptableLanguage (_, Some (Weight w)) -> 1. - w
                              | _ -> 0.)

    let private filter =
        List.filter (function | AcceptableLanguage (_, Some (Weight 0.)) -> false
                              | _ -> true)

    let private map supported =
        List.map (function | AcceptableLanguage (Range x, _) -> List.filter (fun s -> eq s x) supported
                           | AcceptableLanguage (LanguageRange.Any, _) -> supported)
    
    let private run supported =
            sort
         >> filter
         >> map supported
         >> Seq.concat
         >> Seq.distinct
         >> Seq.toList

    let negotiate supported acceptable =
        run (Set.toList supported) (Set.toList acceptable)

(* MediaType *)

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module MediaType =

    let private max (MediaType (Type t, SubType s, _)) =
        function | AcceptableMedia (MediaRange.Closed (Type t', SubType s', _), _) when String.equalsCI t t' && String.equalsCI s s' -> Some 0
                 | AcceptableMedia (MediaRange.Partial (Type t', _), _) when String.equalsCI t t' -> Some 1
                 | AcceptableMedia (MediaRange.Open (_), _) -> Some 2
                 | _ -> None

    let private map requested =
        List.map (fun (x: MediaType) ->
            x, List.chooseMaxBy (max x) requested)

    let private sort =
        List.sortBy (fun (_, y) ->
            (function | Some (AcceptableMedia (_, Some (AcceptParameters (Weight w, _)))) -> 1. - w
                      | _ -> 0.) y)

    let private choose =
        List.choose (fun (x, y) ->
            (function | Some (AcceptableMedia (_, Some (AcceptParameters (Weight w, _)))) when w > 0. -> Some x
                      | Some (AcceptableMedia (_, None)) -> Some x
                      | _ -> None) y)

    let private run requested =
            map requested 
         >> sort
         >> choose

    let negotiate supported acceptable =
        run (Set.toList acceptable) (Set.toList supported)