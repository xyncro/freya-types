namespace Freya.Types.Tests

open System
open System.Globalization
open System.Threading
open Swensen.Unquote

(* Operators *)

module Operators =

    let inline (=?) a b =
        test <@ a = b @>

(* Cultures *)

[<RequireQualifiedAccess>]
module internal Culture =

    let defaults =
        [ CultureInfo ("en")
          CultureInfo ("de") ]

    let runWith culture =
        let current = Thread.CurrentThread.CurrentCulture
        Thread.CurrentThread.CurrentCulture <- culture
        
        { new IDisposable with
            member __.Dispose() =
                Thread.CurrentThread.CurrentCulture <- current }

(* Tests *)

[<AutoOpen>]
module Tests =

    open Operators

    type Iso<'a> =
        ('a -> string) * (string -> 'a)

    let roundTrip<'a when 'a: equality> (iso: Iso<'a>) =
            List.collect (fun p -> List.map (fun x -> x, p) Culture.defaults)
         >> List.iter (fun (c, (a, s)) ->
                use __ = Culture.runWith c
                (fst iso) a =? s
                (snd iso) s =? a)
