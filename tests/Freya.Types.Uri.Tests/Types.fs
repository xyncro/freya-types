module Freya.Types.Uri.Tests

open Freya.Types.Tests
open Freya.Types.Uri
open Xunit

[<Fact>]
let ``Scheme Formatting/Parsing`` () =
    let schemeTyped =
        Scheme "http"

    let schemeString =
        "http"

    roundTrip (Scheme.format, Scheme.parse) [
        schemeTyped, schemeString ]

[<Fact>]
let ``Authority Formatting/Parsing`` () =
    
    (* Host Only *)

    let hostTyped =
        Authority.Authority (IPv4 "192.168.0.1", None, None)

    let hostString =
        "192.168.0.1"

    (* Host and Port *)

    let hostPortTyped =
        Authority.Authority (IPv6 "2001:db8::ff00:42:8329", Some (Port 8080), None)

    let hostPortString =
        "[2001:db8::ff00:42:8329]:8080"

    (* Host, Port and UserInfo *)

    let hostPortUserTyped =
        Authority.Authority (Name (RegName "www.example.com"), Some (Port 8080),Some (UserInfo "user%20name:pass"))

    let hostPortUserString =
        "user%20name:pass@www.example.com:8080"

    (* Round Trip *)

    roundTrip (Authority.format, Authority.parse) [
        hostTyped,         hostString
        hostPortTyped,     hostPortString
        hostPortUserTyped, hostPortUserString ]

[<Fact>]
let ``PathAbsoluteOrEmpty Formatting/Parsing`` () =

    let pathAbEmptyFullTyped = 
        PathAbsoluteOrEmpty [ "some%20value%3F"; "path" ]

    let pathAbEmptyFullString =
        "/some%20value%3F/path"

    let pathAbEmptyEmptyTyped = 
        PathAbsoluteOrEmpty []

    let pathAbEmptyEmptyString =
        ""

    roundTrip (PathAbsoluteOrEmpty.format, PathAbsoluteOrEmpty.parse) [
        pathAbEmptyFullTyped,  pathAbEmptyFullString
        pathAbEmptyEmptyTyped, pathAbEmptyEmptyString ]

[<Fact>]
let ``PathAbsolute Formatting/Parsing`` () =

    let pathAbsoluteFullTyped = 
        PathAbsolute [ "some%20value%3F"; "path" ]

    let pathAbsoluteFullString =
        "/some%20value%3F/path"

    let pathAbsoluteEmptyTyped = 
        PathAbsolute []

    let pathAbsoluteEmptyString =
        "/"

    roundTrip (PathAbsolute.format, PathAbsolute.parse) [
        pathAbsoluteFullTyped,  pathAbsoluteFullString
        pathAbsoluteEmptyTyped, pathAbsoluteEmptyString ]

[<Fact>]
let ``Uri Formatting/Parsing`` () =

    (* File URI *)

    let fileTyped =
        Uri.Uri (
            Scheme "file",
            HierarchyPart.Authority (
                Authority.Authority (Name (RegName ""), None, None),
                PathAbsoluteOrEmpty [ "etc"; "fstab" ]),
            None,
            None)

    let fileString =
        "file:///etc/fstab"

    (* Authority Hierarchy *)
    
    let authorityTyped =
        Uri.Uri (
            Scheme "http",
            HierarchyPart.Authority (
                Authority.Authority (Name (RegName "www.example.com"), Some (Port 8080), Some (UserInfo "user%20name:pass")),
                PathAbsoluteOrEmpty [ "seg1"; "seg2" ]),
            Some (Query "key=some%20value"),
            Some (Fragment "frag1"))

    let authorityString =
        "http://user%20name:pass@www.example.com:8080/seg1/seg2?key=some%20value#frag1"

    (* Rootless Hierarchy *)

    let rootlessTyped =
        Uri.Uri (
            Scheme "urn",
            HierarchyPart.Rootless (PathRootless [ "example:animal:ferret:nose" ]),
            None,
            None)

    let rootlessString =
        "urn:example:animal:ferret:nose"

    (* Absolute Hierarchy *)

    let absoluteTyped =
        Uri.Uri (
            Scheme "sip",
            HierarchyPart.Absolute (PathAbsolute [ "user"; "example" ]),
            None,
            None)

    let absoluteString =
        "sip:/user/example"

    (* Empty Hierarchy *)

    let emptyTyped =
        Uri.Uri (
            Scheme "test",
            HierarchyPart.Empty,
            None,
            None)

    let emptyString =
        "test:"

    (* Round Trip *)

    roundTrip (Uri.format, Uri.parse) [
        fileTyped,      fileString
        authorityTyped, authorityString
        rootlessTyped,  rootlessString
        absoluteTyped,  absoluteString
        emptyTyped,     emptyString ]

[<Fact>]
let ``Query Parse With Encoded Equals`` () =
    let expectedResult = Some [ "one", Some ("two%3d") ]
    let query = Query.parse "one=two%3d"
    let queryPairs = query |> fst Query.pairs_
    Assert.Equal (expectedResult, queryPairs)

[<Fact>]
let ``Query Pairs``() =
    let expectedResult = Some ["param", Some("exists");"param1", Some("alsoexists")]
    let query = Query.Query("param=exists&param1=alsoexists")
    let queryPairs = query |> fst Query.pairs_
    Assert.Equal (expectedResult, queryPairs)

[<Fact>]
let ``Query Pairs with missing parameter value``() =
    let expectedResult = Some ["param", Some("exists");"param1", Some ("")]
    let query = Query.Query("param=exists&param1=")
    let queryPairs = query |> fst Query.pairs_
    Assert.Equal (expectedResult, queryPairs)

[<Fact>]
let ``Query Pairs with missing parameter value and equals sign``() =
    let expectedResult = Some ["param", Some("exists");"param1", None]
    let query = Query.Query("param=exists&param1")
    let queryPairs = query |> fst Query.pairs_
    Assert.Equal (expectedResult, queryPairs)

[<Fact>]
let ``Query Pairs with no query parameters``() =
    let expectedResult = None
    let query = Query.Query("")
    let queryPairs = query |> fst Query.pairs_
    Assert.Equal (expectedResult, queryPairs)

[<Fact>]
let ``Query Pairs with more than 1 & can be parsed``() =
    let expectedResult = Some ["param", Some("exists"); "", None; "param1", Some("alsoexists")]
    let query = Query.Query("param=exists&&param1=alsoexists")
    let queryPairs = query |> fst Query.pairs_
    Assert.Equal (expectedResult, queryPairs)