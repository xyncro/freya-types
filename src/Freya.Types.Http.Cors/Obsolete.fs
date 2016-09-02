namespace Freya.Types.Http.Cors

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module Origin =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Origin.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Origin.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Origin.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlAllowOrigin =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlAllowOrigin.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlAllowOrigin.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlAllowOrigin.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlAllowCredentials =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlAllowCredentials.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlAllowCredentials.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlAllowCredentials.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlExposeHeaders =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlExposeHeaders.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlExposeHeaders.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlExposeHeaders.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlMaxAge =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlMaxAge.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlMaxAge.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlMaxAge.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlAllowMethods =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlAllowMethods.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlAllowMethods.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlAllowMethods.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlAllowHeaders =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlAllowHeaders.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlAllowHeaders.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlAllowHeaders.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlRequestMethod =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlRequestMethod.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlRequestMethod.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlRequestMethod.tryParse

    [<RequireQualifiedAccess>]
    module AccessControlRequestHeaders =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AccessControlRequestHeaders.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AccessControlRequestHeaders.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AccessControlRequestHeaders.tryParse