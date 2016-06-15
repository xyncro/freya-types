module Freya.Types.Http.State.Tests

open System
open Freya.Types.Http.State
open Freya.Types.Tests
open Xunit

[<Fact>]
let ``Cookie Formatting/Parsing`` () =
    let cookieTyped =
        Cookie [
            Pair (Name "test", Value "value") ]

    let cookieString =
        "test=value"

    roundTrip (Cookie.format, Cookie.parse) [
        cookieTyped, cookieString ]

[<Fact>]
let ``Multiple Cookie Formatting/Parsing`` () =
    let cookieTyped =
        Cookie [
            Pair (Name "cookie1", Value "foo")
            Pair (Name "cookie2", Value "bar") ]

    let cookieString =
        "cookie1=foo; cookie2=bar"

    roundTrip (Cookie.format, Cookie.parse) [
        cookieTyped, cookieString ]

[<Fact>]
let ``Set-Cookie Formatting/Parsing`` () =
    let setCookieTyped =
        SetCookie (
            Pair (Name "test", Value "value"),
            Attributes [
                Expires (DateTime.Parse "1994/10/29 19:43:31")
                MaxAge (TimeSpan.FromSeconds 42.)
                Domain (SubDomain "www.example.com")
                Path ("/some/path")
                Secure
                HttpOnly ])

    let setCookieString =
        "test=value; Expires=Sat, 29 Oct 1994 19:43:31 GMT; Max-Age=42; Domain=www.example.com; Path=/some/path; Secure; HttpOnly"

    roundTrip (SetCookie.format, SetCookie.parse) [
        setCookieTyped, setCookieString ]