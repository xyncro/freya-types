namespace Freya.Types.Language

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module Language =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Language.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Language.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Language.tryParse

    [<RequireQualifiedAccess>]
    module LanguageTag =

        [<Obsolete ("Use format instead.")>]
        let Format =
            LanguageTag.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            LanguageTag.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            LanguageTag.tryParse

    [<RequireQualifiedAccess>]
    module LanguageRange =

        [<Obsolete ("Use format instead.")>]
        let Format =
            LanguageRange.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            LanguageRange.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            LanguageRange.tryParse