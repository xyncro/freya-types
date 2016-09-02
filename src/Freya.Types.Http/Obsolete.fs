namespace Freya.Types.Http

open System

(* Obsolete

   Backwards compatibility shims to make the 2.x-> 3.x transition
   less painful, providing functionally equivalent options where possible.

   To be removed for 4.x releases. *)

[<AutoOpen>]
module Obsolete =

    [<RequireQualifiedAccess>]
    module PartialUri =

        [<Obsolete ("Use format instead.")>]
        let Format =
            PartialUri.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            PartialUri.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            PartialUri.tryParse

    [<RequireQualifiedAccess>]
    module HttpVersion =

        [<Obsolete ("Use format instead.")>]
        let Format =
            HttpVersion.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            HttpVersion.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            HttpVersion.tryParse

    [<RequireQualifiedAccess>]
    module ContentLength =

        [<Obsolete ("Use format instead.")>]
        let Format =
            ContentLength.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            ContentLength.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            ContentLength.tryParse

    [<RequireQualifiedAccess>]
    module Host =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Host.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Host.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Host.tryParse

    [<RequireQualifiedAccess>]
    module Connection =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Connection.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Connection.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Connection.tryParse

    [<RequireQualifiedAccess>]
    module MediaType =

        [<Obsolete ("Use type_ instead.")>]
        let Type_ =
            MediaType.type_

        [<Obsolete ("Use subType_ instead.")>]
        let SubType_ =
            MediaType.subType_

        [<Obsolete ("Use parameters_ instead.")>]
        let Parameters_ =
            MediaType.parameters_

        [<Obsolete ("Use format instead.")>]
        let Format =
            MediaType.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            MediaType.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            MediaType.tryParse

    [<RequireQualifiedAccess>]
    module Parameters =

        [<Obsolete ("Use parameters_ instead.")>]
        let Parameters_ =
            Parameters.parameters_

    [<RequireQualifiedAccess>]
    module ContentType =

        [<Obsolete ("Use mediaType_ instead.")>]
        let MediaType_ =
            ContentType.mediaType_

        [<Obsolete ("Use format instead.")>]
        let Format =
            ContentType.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            ContentType.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            ContentType.tryParse

    [<RequireQualifiedAccess>]
    module ContentEncoding =

        [<Obsolete ("Use format instead.")>]
        let Format =
            ContentEncoding.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            ContentEncoding.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            ContentEncoding.tryParse

    [<RequireQualifiedAccess>]
    module ContentLanguage =

        [<Obsolete ("Use format instead.")>]
        let Format =
            ContentLanguage.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            ContentLanguage.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            ContentLanguage.tryParse

    [<RequireQualifiedAccess>]
    module ContentLocation =

        [<Obsolete ("Use format instead.")>]
        let Format =
            ContentLocation.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            ContentLocation.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            ContentLocation.tryParse

    [<RequireQualifiedAccess>]
    module Method =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Method.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Method.parse

    [<RequireQualifiedAccess>]
    module Expect =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Expect.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Expect.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Expect.tryParse

    [<RequireQualifiedAccess>]
    module MaxForwards =

        [<Obsolete ("Use format instead.")>]
        let Format =
            MaxForwards.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            MaxForwards.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            MaxForwards.tryParse

    [<RequireQualifiedAccess>]
    module Accept =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Accept.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Accept.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Accept.tryParse

    [<RequireQualifiedAccess>]
    module AcceptCharset =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AcceptCharset.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AcceptCharset.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AcceptCharset.tryParse

    [<RequireQualifiedAccess>]
    module AcceptEncoding =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AcceptEncoding.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AcceptEncoding.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AcceptEncoding.tryParse

    [<RequireQualifiedAccess>]
    module AcceptLanguage =

        [<Obsolete ("Use format instead.")>]
        let Format =
            AcceptLanguage.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            AcceptLanguage.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            AcceptLanguage.tryParse

    [<RequireQualifiedAccess>]
    module Referer =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Referer.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Referer.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Referer.tryParse

    [<RequireQualifiedAccess>]
    module Date =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Date.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Date.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Date.tryParse

    [<RequireQualifiedAccess>]
    module Location =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Location.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Location.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Location.tryParse

    [<RequireQualifiedAccess>]
    module RetryAfter =

        [<Obsolete ("Use format instead.")>]
        let Format =
            RetryAfter.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            RetryAfter.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            RetryAfter.tryParse

    [<RequireQualifiedAccess>]
    module Allow =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Allow.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Allow.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Allow.tryParse

    [<RequireQualifiedAccess>]
    module LastModified =

        [<Obsolete ("Use format instead.")>]
        let Format =
            LastModified.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            LastModified.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            LastModified.tryParse

    [<RequireQualifiedAccess>]
    module ETag =

        [<Obsolete ("Use format instead.")>]
        let Format =
            ETag.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            ETag.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            ETag.tryParse

    [<RequireQualifiedAccess>]
    module IfMatch =

        [<Obsolete ("Use format instead.")>]
        let Format =
            IfMatch.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            IfMatch.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            IfMatch.tryParse

    [<RequireQualifiedAccess>]
    module IfNoneMatch =

        [<Obsolete ("Use format instead.")>]
        let Format =
            IfNoneMatch.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            IfNoneMatch.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            IfNoneMatch.tryParse

    [<RequireQualifiedAccess>]
    module IfModifiedSince =

        [<Obsolete ("Use format instead.")>]
        let Format =
            IfModifiedSince.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            IfModifiedSince.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            IfModifiedSince.tryParse

    [<RequireQualifiedAccess>]
    module IfUnmodifiedSince =

        [<Obsolete ("Use format instead.")>]
        let Format =
            IfUnmodifiedSince.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            IfUnmodifiedSince.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            IfUnmodifiedSince.tryParse

    [<RequireQualifiedAccess>]
    module IfRange =

        [<Obsolete ("Use format instead.")>]
        let Format =
            IfRange.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            IfRange.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            IfRange.tryParse

    [<RequireQualifiedAccess>]
    module Age =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Age.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Age.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Age.tryParse

    [<RequireQualifiedAccess>]
    module CacheControl =

        [<Obsolete ("Use format instead.")>]
        let Format =
            CacheControl.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            CacheControl.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            CacheControl.tryParse

    [<RequireQualifiedAccess>]
    module Expires =

        [<Obsolete ("Use format instead.")>]
        let Format =
            Expires.format

        [<Obsolete ("Use parse instead.")>]
        let Parse =
            Expires.parse

        [<Obsolete ("Use tryParse instead.")>]
        let TryParse =
            Expires.tryParse