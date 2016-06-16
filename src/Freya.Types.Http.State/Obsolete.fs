namespace Freya.Types.Http.State

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module Pair =

        [<Obsolete ("Use name_ instead.")>]
        let Name_ =
            Pair.name_

        [<Obsolete ("Use value_ instead.")>]
        let Value_ =
            Pair.value_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Pair.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Pair.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Pair.tryParse

    [<RequireQualifiedAccess>]
    module Name =

        [<Obsolete ("Use name_ instead.")>]
        let Name_ =
            Name.name_

    [<RequireQualifiedAccess>]
    module Value =

        [<Obsolete ("Use value_ instead.")>]
        let Value_ =
            Value.value_

    [<RequireQualifiedAccess>]
    module SetCookie =

        [<Obsolete ("Use pair_ instead.")>]
        let Pair_ =
            SetCookie.pair_

        [<Obsolete ("Use attributes_ instead.")>]
        let Attributes_ =
            SetCookie.attributes_

        [<Obsolete ("Use format instead.")>]
        let Format =
            SetCookie.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            SetCookie.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            SetCookie.tryParse

    [<RequireQualifiedAccess>]
    module Attributes =

        [<Obsolete ("Use attributes_ instead.")>]
        let Attributes_ =
            Attributes.attributes_

    [<RequireQualifiedAccess>]
    module Domain =

        [<Obsolete ("Use ipv4_ instead.")>]
        let IPv4_ =
            Domain.ipv4_

        [<Obsolete ("Use ipv6_ instead.")>]
        let IPv6_ =
            Domain.ipv6_

        [<Obsolete ("Use subDomain_ instead.")>]
        let SubDomain_ =
            Domain.subDomain_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Domain.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Domain.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Domain.tryParse

    [<RequireQualifiedAccess>]
    module Cookie =

        [<Obsolete ("Use pairs_ instead.")>]
        let Pairs_ =
            Cookie.pairs_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Cookie.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Cookie.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Cookie.tryParse