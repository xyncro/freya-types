module Freya.Types.Cors.Tests

open System
open Freya.Types.Http
open Freya.Types.Http.Cors
open Freya.Types.Tests
open Freya.Types.Uri
open Xunit

[<Fact>]
let ``Origin Formatting/Parsing`` () =
    let originTyped =
        Origin (
            OriginListOrNull.Origins [
                SerializedOrigin (
                    Scheme "http",
                    Name (RegName "www.example.com"),
                    Some (Port 8080)) ])

    let originString =
        "http://www.example.com:8080"

    roundTrip (Origin.format, Origin.parse) [
        originTyped, originString ]

[<Fact>]
let ``AccessControlAllowOrigin Formatting/Parsing`` () =
    let accessControlAllowOriginTyped =
        AccessControlAllowOrigin (
            Origins (
                OriginListOrNull.Origins [
                    SerializedOrigin (
                        Scheme "http",
                        Name (RegName "www.example.com"),
                        Some (Port 8080)) ]))

    let accessControlAllowOriginString =
        "http://www.example.com:8080"

    roundTrip (AccessControlAllowOrigin.format, AccessControlAllowOrigin.parse) [
        accessControlAllowOriginTyped, accessControlAllowOriginString ]

[<Fact>]
let ``AccessControlAllowCredentials Formatting/Parsing`` () =
    let accessControlAllowCredentialsTyped =
        AccessControlAllowCredentials

    let accessControlAllowCredentialsString =
        "true"

    roundTrip (AccessControlAllowCredentials.format, AccessControlAllowCredentials.parse) [
        accessControlAllowCredentialsTyped, accessControlAllowCredentialsString ]

[<Fact>]
let ``AccessControlExposeHeaders Formatting/Parsing`` () =
    let accessControlExposeHeadersTyped =
        AccessControlExposeHeaders [ "X-Custom-Header"; "X-Another-Header" ]

    let accessControlExposeHeadersString =
        "X-Custom-Header,X-Another-Header"

    roundTrip (AccessControlExposeHeaders.format, AccessControlExposeHeaders.parse) [
        accessControlExposeHeadersTyped, accessControlExposeHeadersString ]

[<Fact>]
let ``AccessControlMaxAge Formatting/Parsing`` () =
    let accessControlMaxAgeTyped =
        AccessControlMaxAge (TimeSpan.FromSeconds (1024.))

    let accessControlMaxAgeString =
        "1024"

    roundTrip (AccessControlMaxAge.format, AccessControlMaxAge.parse) [
        accessControlMaxAgeTyped, accessControlMaxAgeString ]

[<Fact>]
let ``AccessControlAllowMethods Formatting/Parsing`` () =
    let accessControlAllowMethodsTyped =
        AccessControlAllowMethods [ DELETE; PUT ]

    let accessControlAllowMethodsString =
        "DELETE,PUT"

    roundTrip (AccessControlAllowMethods.format, AccessControlAllowMethods.parse) [
        accessControlAllowMethodsTyped, accessControlAllowMethodsString ]

[<Fact>]
let ``AccessControlAllowHeaders Formatting/Parsing`` () =
    let accessControlAllowHeadersTyped =
        AccessControlAllowHeaders [ "X-Custom-Header"; "X-Another-Header" ]

    let accessControlAllowHeadersString =
        "X-Custom-Header,X-Another-Header"

    roundTrip (AccessControlAllowHeaders.format, AccessControlAllowHeaders.parse) [
        accessControlAllowHeadersTyped, accessControlAllowHeadersString ]

[<Fact>]
let ``AccessControlRequestMethod Formatting/Parsing`` () =
    let accessControlRequestMethodTyped =
        AccessControlRequestMethod DELETE

    let accessControlRequestMethodString =
        "DELETE"

    roundTrip (AccessControlRequestMethod.format, AccessControlRequestMethod.parse) [
        accessControlRequestMethodTyped, accessControlRequestMethodString ]

[<Fact>]
let ``AccessControlRequestHeaders Formatting/Parsing`` () =
    let accessControlRequestHeadersTyped =
        AccessControlRequestHeaders [ "X-Custom-Header"; "X-Another-Header" ]

    let accessControlRequestHeadersString =
        "X-Custom-Header,X-Another-Header"

    roundTrip (AccessControlRequestHeaders.format, AccessControlRequestHeaders.parse) [
        accessControlRequestHeadersTyped, accessControlRequestHeadersString ]