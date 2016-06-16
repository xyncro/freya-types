namespace Freya.Types.Uri.Template

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module UriTemplateData =

        [<Obsolete ("Use uriTemplateData_ instead.")>]
        let UriTemplateData_ =
            UriTemplateData.uriTemplateData_

    [<RequireQualifiedAccess>]
    module UriTemplateValue =

        [<Obsolete ("Use atom_ instead.")>]
        let Atom_ =
            UriTemplateValue.atom_

        [<Obsolete ("Use list_ instead.")>]
        let List_ =
            UriTemplateValue.list_

        [<Obsolete ("Use keys_ instead.")>]
        let Keys_ =
            UriTemplateValue.keys_

    [<RequireQualifiedAccess>]
    module UriTemplate =

        [<Obsolete ("Use format instead.")>]
        let Format =
            UriTemplate.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            UriTemplate.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            UriTemplate.tryParse

    [<RequireQualifiedAccess>]
    module UriTemplatePart =

        [<Obsolete ("Use format instead.")>]
        let Format =
            UriTemplatePart.format