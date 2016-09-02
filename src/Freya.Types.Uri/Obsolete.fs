namespace Freya.Types.Uri

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module Scheme =

        [<Obsolete ("Use scheme_ instead.")>]
        let Scheme_ =
            Scheme.scheme_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Scheme.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Scheme.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Scheme.tryParse

    [<RequireQualifiedAccess>]
    module Authority =

        [<Obsolete ("Use host_ instead.")>]
        let Host_ =
            Authority.host_

        [<Obsolete ("Use port_ instead.")>]
        let Port_ =
            Authority.port_

        [<Obsolete ("Use userInfo_ instead.")>]
        let UserInfo_ =
            Authority.userInfo_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Authority.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Authority.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Authority.tryParse

    [<RequireQualifiedAccess>]
    module Host =

        [<Obsolete ("Use ipv4_ instead.")>]
        let IPv4_ =
            Host.ipv4_

        [<Obsolete ("Use ipv6_ instead.")>]
        let IPv6_ =
            Host.ipv6_

        [<Obsolete ("Use name_ instead.")>]
        let Name_ =
            Host.name_

    [<RequireQualifiedAccess>]
    module Port =

        [<Obsolete ("Use port_ instead.")>]
        let Port_ =
            Port.port_

    [<RequireQualifiedAccess>]
    module PathAbsoluteOrEmpty =

        [<Obsolete ("Use format instead.")>]
        let Format =
            PathAbsoluteOrEmpty.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            PathAbsoluteOrEmpty.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            PathAbsoluteOrEmpty.tryParse

    [<RequireQualifiedAccess>]
    module PathAbsolute =

        [<Obsolete ("Use format instead.")>]
        let Format =
            PathAbsolute.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            PathAbsolute.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            PathAbsolute.tryParse

    [<RequireQualifiedAccess>]
    module PathNoScheme =

        [<Obsolete ("Use format instead.")>]
        let Format =
            PathNoScheme.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            PathNoScheme.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            PathNoScheme.tryParse

    [<RequireQualifiedAccess>]
    module PathRootless =

        [<Obsolete ("Use format instead.")>]
        let Format =
            PathRootless.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            PathRootless.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            PathRootless.tryParse

    [<RequireQualifiedAccess>]
    module Query =

        [<Obsolete ("User pairs_ instead.")>]
        let Pairs_ =
            Query.pairs_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Query.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Query.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Query.tryParse

    [<RequireQualifiedAccess>]
    module Fragment =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Fragment.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Fragment.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Fragment.tryParse

    [<RequireQualifiedAccess>]
    module Uri =

        [<Obsolete ("Use scheme_ instead.")>]
        let Scheme_ =
            Uri.scheme_

        [<Obsolete ("Use hierarchyPart_ instead.")>]
        let HierarchyPart_ =
            Uri.hierarchyPart_

        [<Obsolete ("Use query_ instead.")>]
        let Query_ =
            Uri.query_

        [<Obsolete ("Use fragment_ instead.")>]
        let Fragment_ =
            Uri.fragment_

        [<Obsolete ("Use format instead.")>]
        let Format =
            Uri.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Uri.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Uri.tryParse

    [<RequireQualifiedAccess>]
    module HierarchyPart =

        [<Obsolete ("Use authority_ instead.")>]
        let Authority_ =
            HierarchyPart.authority_

        [<Obsolete ("Use absolute_ instead.")>]
        let Absolute_ =
            HierarchyPart.absolute_

        [<Obsolete ("Use rootless_ instead.")>]
        let Rootless_ =
            HierarchyPart.rootless_

        [<Obsolete ("Use empty_ instead.")>]
        let Empty_ =
            HierarchyPart.empty_

    [<RequireQualifiedAccess>]
    module RelativeReference =

        [<Obsolete ("Use relativePart_ instead.")>]
        let RelativePart_ =
            RelativeReference.relativePart_

        [<Obsolete ("Use query_ instead.")>]
        let Query_ =
            RelativeReference.query_

        [<Obsolete ("Use fragment_ instead.")>]
        let Fragment_ =
            RelativeReference.fragment_

        [<Obsolete ("Use format instead.")>]
        let Format =
            RelativeReference.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            RelativeReference.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            RelativeReference.tryParse

    [<RequireQualifiedAccess>]
    module RelativePart =

        [<Obsolete ("Use authority_ instead.")>]
        let Authority_ =
            RelativePart.authority_

        [<Obsolete ("Use absolute_ instead.")>]
        let Absolute_ =
            RelativePart.absolute_

        [<Obsolete ("Use noScheme_ instead.")>]
        let NoScheme_ =
            RelativePart.noScheme_

        [<Obsolete ("Use empty_ instead.")>]
        let Empty_ =
            RelativePart.empty_

    [<RequireQualifiedAccess>]
    module AbsoluteUri =

        [<Obsolete ("Use scheme_ instead.")>]
        let Scheme_ =
            AbsoluteUri.scheme_

        [<Obsolete ("Use hierarchyPart_ instead.")>]
        let HierarchyPart_ =
            AbsoluteUri.hierarchyPart_

        [<Obsolete ("Use query_ instead.")>]
        let Query_ =
            AbsoluteUri.query_

        [<Obsolete ("Use format instead.")>]
        let Format =
            AbsoluteUri.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AbsoluteUri.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AbsoluteUri.tryParse

    [<RequireQualifiedAccess>]
    module UriReference =

        [<Obsolete ("Use uri_ instead.")>]
        let Uri_ =
            UriReference.uri_

        [<Obsolete ("Use relative_ instead.")>]
        let Relative_ =
            UriReference.relative_

        [<Obsolete ("Use format instead.")>]
        let Format =
            UriReference.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            UriReference.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            UriReference.tryParse