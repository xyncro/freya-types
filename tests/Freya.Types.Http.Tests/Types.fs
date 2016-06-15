module Freya.Types.Http.Tests

open System
open Freya.Types.Http
open Freya.Types.Language
open Freya.Types.Tests
open Freya.Types.Uri
open Xunit

(* RFC 7230 *)

[<Fact>]
let ``PartialUri Formatting/Parsing`` () =

    let partialUriTyped : PartialUri =
        PartialUri (
            RelativePart.Absolute (PathAbsolute [ "some"; "path" ]),
            Some (Query "key=val"))

    let partialUriString =
        "/some/path?key=val"

    roundTrip (PartialUri.format, PartialUri.parse) [
        partialUriTyped, partialUriString ]

[<Fact>]
let ``HttpVersion Formatting/Parsing`` () =
    
    let httpVersionTyped = HTTP 1.1
    let httpVersionString = "HTTP/1.1"

    roundTrip (HttpVersion.format, HttpVersion.parse) [
        httpVersionTyped, httpVersionString ]

[<Fact>]
let ``ContentLength Formatting/Parsing`` () =
    
    let contentLengthTyped = ContentLength 1024
    let contentLengthString = "1024"

    roundTrip (ContentLength.format, ContentLength.parse) [
        contentLengthTyped, contentLengthString ]

[<Fact>]
let ``Host Formatting/Parsing`` () =

    let hostTyped = Host (Name (RegName "www.example.com"), Some (Port 8080))
    let hostString = "www.example.com:8080"

    roundTrip (Host.format, Host.parse) [
        hostTyped, hostString ]

[<Fact>]
let ``Connection Formatting/Parsing`` () =

    let connectionTyped = Connection ([ ConnectionOption "close"; ConnectionOption "test" ])
    let connectionString = "close,test"

    roundTrip (Connection.format, Connection.parse) [
        connectionTyped, connectionString ]

(* RFC 7231 *)

[<Fact>]
let ``MediaType Formatting/Parsing`` () =

    let mediaTypeTyped =
        MediaType (
            Type "application",
            SubType "json", 
            Parameters (Map.ofList [ "charset", "utf-8" ]))

    let mediaTypeString =
        "application/json;charset=utf-8"

    roundTrip (MediaType.format, MediaType.parse) [
        mediaTypeTyped, mediaTypeString ]

[<Fact>]
let ``ContentType Formatting/Parsing`` () =

    let contentTypeTyped =
        ContentType (
            MediaType (
                Type "application",
                SubType "json",
                Parameters (Map.ofList [ "charset", "utf-8" ])))

    let contentTypeString =
        "application/json;charset=utf-8"

    roundTrip (ContentType.format, ContentType.parse) [
        contentTypeTyped, contentTypeString ]

[<Fact>]
let ``ContentEncoding Formatting/Parsing`` () =

    let contentEncodingTyped =
        ContentEncoding [ 
            ContentCoding "compress"
            ContentCoding "deflate" ]

    let contentEncodingString =
        "compress,deflate"

    roundTrip (ContentEncoding.format, ContentEncoding.parse) [
        contentEncodingTyped, contentEncodingString ]

[<Fact>]
let ``ContentLanguage Formatting/Parsing`` () =

    let contentLanguageTyped =
        ContentLanguage [
            LanguageTag (
                Language ("en", None),
                None,
                Some (Region "GB"),
                Variant [])
            LanguageTag (
                Language ("hy", None),
                Some (Script "Latn"),
                Some (Region "IT"),
                Variant [ "arvela" ]) ]

    let contentLanguageString =
        "en-GB,hy-Latn-IT-arvela"

    roundTrip (ContentLanguage.format, ContentLanguage.parse) [
        contentLanguageTyped, contentLanguageString ]

[<Fact>]
let ``ContentLocation Formatting/Parsing`` () =
    
    let contentLocationTyped =
        ContentLocation (
            ContentLocationUri.Absolute (
                AbsoluteUri (
                    Scheme "http",
                    HierarchyPart.Absolute (PathAbsolute [ "some"; "path" ]),
                    None)))

    let contentLocationString =
        "http:/some/path"

    roundTrip (ContentLocation.format, ContentLocation.parse) [
        contentLocationTyped, contentLocationString ]

[<Fact>]
let ``Method Formatting/Parsing`` () =

    roundTrip (Method.format, Method.parse) [
        Method.GET, "GET"
        Method.Custom "PATCH", "PATCH" ]

[<Fact>]
let ``Expect Formatting/Parsing`` () =

    roundTrip (Expect.format, Expect.parse) [
        Expect (Continue), "100-continue" ]

[<Fact>]
let ``MaxForwards Formatting/Parsing`` () =
    
    roundTrip (MaxForwards.format, MaxForwards.parse) [
        MaxForwards 10, "10" ]

[<Fact>]
let ``Accept Formatting/Parsing`` () =

    let acceptTyped =
        Accept [
            AcceptableMedia (
                Closed (Type "application", SubType "json", Parameters Map.empty),
                Some (AcceptParameters (Weight 0.7, Extensions Map.empty)))
            AcceptableMedia (
                MediaRange.Partial (Type "text", Parameters Map.empty),
                Some (AcceptParameters (Weight 0.5, Extensions Map.empty)))
            AcceptableMedia (
                Open (Parameters Map.empty),
                None) ]

    let acceptString =
        "application/json;q=0.7,text/*;q=0.5,*/*"

    roundTrip (Accept.format, Accept.parse) [
        acceptTyped, acceptString ]

[<Fact>]
let ``AcceptCharset Formatting/Parsing`` () =
    
    let acceptCharsetTyped =
        AcceptCharset [
            AcceptableCharset (
                CharsetRange.Charset (Charset.Utf8),
                Some (Weight 0.7))
            AcceptableCharset (
                CharsetRange.Any,
                Some (Weight 0.2)) ]

    let acceptCharsetString =
        "utf-8;q=0.7,*;q=0.2"

    roundTrip (AcceptCharset.format, AcceptCharset.parse) [
        acceptCharsetTyped, acceptCharsetString ]

[<Fact>]
let ``AcceptEncoding Formatting/Parsing`` () =

    let acceptEncodingTyped =
        AcceptEncoding [
            AcceptableEncoding (
                EncodingRange.Coding (ContentCoding.Compress),
                Some (Weight 0.8))
            AcceptableEncoding (
                EncodingRange.Identity,
                None)
            AcceptableEncoding (
                EncodingRange.Any,
                Some (Weight 0.3)) ]

    let acceptEncodingString =
        "compress;q=0.8,identity,*;q=0.3"

    roundTrip (AcceptEncoding.format, AcceptEncoding.parse) [
        acceptEncodingTyped, acceptEncodingString ]

[<Fact>]
let ``AcceptLanguage Formatting/Parsing`` () =

    let acceptLanguageTyped =
        AcceptLanguage [
            AcceptableLanguage (
                Range [ "en"; "GB" ],
                Some (Weight 0.8))
            AcceptableLanguage (
                Any,
                None) ]

    let acceptLanguageString =
        "en-GB;q=0.8,*"

    roundTrip (AcceptLanguage.format, AcceptLanguage.parse) [
        acceptLanguageTyped, acceptLanguageString ]

[<Fact>]
let ``Referer Formatting/Parsing`` () =

    let refererTyped =
        Referer (
            RefererUri.Partial (
                PartialUri (
                    RelativePart.Absolute (PathAbsolute ["some"; "path" ]),
                    None)))

    let refererString =
        "/some/path"

    roundTrip (Referer.format, Referer.parse) [
        refererTyped, refererString ]

[<Fact>]
let ``Date Formatting/Parsing`` () =

    let dateTyped =
        Date.Date (DateTime.Parse ("1994/10/29 19:43:31"))

    let dateString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (Date.format, Date.parse) [
        dateTyped, dateString ]

[<Fact>]
let ``Location Formatting/Parsing`` () =

    let locationTyped =
        Location (
            UriReference.Uri (
                Uri.Uri (
                    Scheme "http",
                    HierarchyPart.Absolute (PathAbsolute [ "some"; "path" ]),
                    None,
                    None)))

    let locationString =
        "http:/some/path"

    roundTrip (Location.format, Location.parse) [
        locationTyped, locationString ]

[<Fact>]
let ``RetryAfter Formatting/Parsing`` () =

    let retryAfterTyped =
        RetryAfter (Delay (TimeSpan.FromSeconds (float 60)))

    let retryAfterString =
        "60"

    roundTrip (RetryAfter.format, RetryAfter.parse) [
        retryAfterTyped, retryAfterString ]

[<Fact>]
let ``Allow Formatting/Parsing`` () =

    let allowTyped =
        Allow [ DELETE; GET; POST; PUT ]

    let allowString =
        "DELETE,GET,POST,PUT"

    roundTrip (Allow.format, Allow.parse) [
        allowTyped, allowString ]

(* RFC 7232 *)

[<Fact>]
let ``LastModified Formatting/Parsing`` () =

    let lastModifiedTyped =
        LastModified (DateTime.Parse ("1994/10/29 19:43:31"))

    let lastModifiedString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (LastModified.format, LastModified.parse) [
        lastModifiedTyped, lastModifiedString ]

[<Fact>]
let ``ETag Formatting/Parsing`` () =

    let eTagTyped =
        ETag (Strong "sometag")

    let eTagString =
        "\"sometag\""

    roundTrip (ETag.format, ETag.parse) [
        eTagTyped, eTagString ]

[<Fact>]
let ``IfMatch Formatting/Parsing`` () =

    let ifMatchTyped =
        IfMatch (IfMatchChoice.EntityTags [ Strong "sometag"; Weak "othertag" ])

    let ifMatchString =
        "\"sometag\",W/\"othertag\""

    roundTrip (IfMatch.format, IfMatch.parse) [
        ifMatchTyped, ifMatchString ]

[<Fact>]
let ``IfNoneMatch Formatting/Parsing`` () =

    let ifNoneMatchTyped =
        IfNoneMatch (IfNoneMatchChoice.EntityTags [ Strong "sometag"; Weak "othertag" ])

    let ifNoneMatchString =
        "\"sometag\",W/\"othertag\""

    roundTrip (IfNoneMatch.format, IfNoneMatch.parse) [
        ifNoneMatchTyped, ifNoneMatchString ]

[<Fact>]
let ``IfModifiedSince Formatting/Parsing`` () =

    let ifModifiedSinceTyped =
        IfModifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))

    let ifModifiedSinceString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfModifiedSince.format, IfModifiedSince.parse) [
        ifModifiedSinceTyped, ifModifiedSinceString ]

[<Fact>]
let ``IfUnmodifiedSince Formatting/Parsing`` () =

    let ifUnmodifiedSinceTyped =
        IfUnmodifiedSince (DateTime.Parse ("1994/10/29 19:43:31"))

    let ifUnmodifiedSinceString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfUnmodifiedSince.format, IfUnmodifiedSince.parse) [
        ifUnmodifiedSinceTyped, ifUnmodifiedSinceString ]

(* RFC 7233 *)

[<Fact>]
let ``IfRange Formatting/Parsing`` () =

    let ifRangeTyped =
        IfRange (IfRangeChoice.Date (DateTime.Parse ("1994/10/29 19:43:31")))

    let ifRangeString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (IfRange.format, IfRange.parse) [
        ifRangeTyped, ifRangeString ]

(* RFC 7234 *)

[<Fact>]
let ``Age Formatting/Parsing`` () =

    let ageTyped =
        Age (TimeSpan.FromSeconds (float 1024))

    let ageString =
        "1024"

    roundTrip (Age.format, Age.parse) [
        ageTyped, ageString ]

[<Fact>]
let ``CacheControl Formatting/Parsing`` () =

    let cacheControlTyped =
        CacheControl [
            MaxAge (TimeSpan.FromSeconds (float 1024))
            NoCache
            Private ]

    let cacheControlString =
        "max-age=1024,no-cache,private"

    roundTrip (CacheControl.format, CacheControl.parse) [
        cacheControlTyped, cacheControlString ]

[<Fact>]
let ``Expires Formatting/Parsing`` () =

    let expiresTyped =
        Expires (DateTime.Parse ("1994/10/29 19:43:31"))

    let expiresString =
        "Sat, 29 Oct 1994 19:43:31 GMT"

    roundTrip (Expires.format, Expires.parse) [
        expiresTyped, expiresString ]