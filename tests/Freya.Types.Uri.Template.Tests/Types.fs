module Freya.Types.Uri.Templates.Tests

open Freya.Types.Tests
open Freya.Types.Tests.Operators
open Freya.Types.Uri.Template
open Xunit

(* Data

   Common superset of data items used throughout RFC 6570
   to frame examples. *)

let data =
    UriTemplateData (
        Map.ofList [
            Key "count",      List [ "one"; "two"; "three" ]
            Key "dom",        List [ "example"; "com" ]
            Key "dub",        Atom "me/too"
            Key "hello",      Atom "Hello World!"
            Key "half",       Atom "50%"
            Key "var",        Atom "value"
            Key "who",        Atom "fred"
            Key "base",       Atom "http://example.com/home/"
            Key "path",       Atom "/foo/bar"
            Key "list",       List [ "red"; "green"; "blue" ]
            Key "keys",       Keys [ ("semi", ";"); ("dot", "."); ("comma", ",") ]
            Key "v",          Atom "6"
            Key "x",          Atom "1024"
            Key "y",          Atom "768"
            Key "empty",      Atom ""
            Key "empty_keys", Keys [] ])

let matches uri path data =
    UriTemplate.parse(uri).Match(path) =? UriTemplateData (Map.ofList data)

let (=?) str1 str2 =
    UriTemplate.parse(str1).Render(data) =? str2


(* Illustrative Examples

   Examples used as part of the overview of URI Templates,
   giving a non-exhaustive flavour of URI Template expansion
   and variables/operators/modifiers. *)

[<Fact>]
let ``Level 1 Examples Render Correctly`` () =

    (* Simple String Expansion *)

    "{var}" =? "value"
    "{hello}" =? "Hello%20World%21"

[<Fact>]
let ``Level 2 Examples Render Correctly`` () =

    (* Reserved String Expansion *)

    "{+var}" =? "value"
    "{+hello}" =? "Hello%20World!"
    "{+path}/here" =? "/foo/bar/here"
    "here?ref={+path}" =? "here?ref=/foo/bar"

    (* Fragment Expansion *)

    "X{#var}" =? "X#value"
    "X{#hello}" =? "X#Hello%20World!"

(* Specifcation Examples

   Examples given as part of individual specification sections
   to drive the full set of expansion/modification cases, forming
   a specification by example in addition to the grammars and
   behaviours given as part of the specification. *)

[<Fact>]
let ``Simple Expansion Renders Correctly`` () =
    "{var}" =? "value"
    "{hello}" =? "Hello%20World%21"
    "{half}" =? "50%25"
    "O{empty}X" =? "OX"
    "O{undef}X" =? "OX"
    "{x,y}" =? "1024,768"
    "{x,hello,y}" =? "1024,Hello%20World%21,768"
    "?{x,empty}" =? "?1024,"
    "?{x,undef}" =? "?1024"
    "?{undef,y}" =? "?768"
    "{var:3}" =? "val"
    "{var:30}" =? "value"
    "{list}" =? "red,green,blue"
    "{list*}" =? "red,green,blue"
    "{keys}" =? "semi,%3B,dot,.,comma,%2C"
    "{keys*}" =? "semi=%3B,dot=.,comma=%2C"

[<Fact>]
let ``Simple Matching Matches Correctly`` () =
    matches "/test/{atom}" "/test/one" [ Key "atom", Atom "one" ]
    matches "/test/{atom}" "/test/" [ Key "atom", Atom "" ]
    matches "/test/{one}/{two}" "/test//two" [ Key "one", Atom ""; Key "two", Atom "two" ]
    matches "/test/{list*}" "/test/one,two,three" [ Key "list", List [ "one"; "two"; "three" ] ]
    matches "/test/{keys*}" "/test/one=a,two=b" [ Key "keys", Keys [ "one", "a"; "two", "b" ] ]

[<Fact>]
let ``Reserved Expansion Renders Correctly`` () =
    "{+var}" =? "value"
    "{+hello}" =? "Hello%20World!"
    "{+half}" =? "50%25"
    "{base}index" =? "http%3A%2F%2Fexample.com%2Fhome%2Findex"
    "{+base}index" =? "http://example.com/home/index"
    "O{+empty}X" =? "OX"
    "O{+undef}X" =? "OX"
    "{+path}/here" =? "/foo/bar/here"
    "here?ref={+path}" =? "here?ref=/foo/bar"
    "up{+path}{var}/here" =? "up/foo/barvalue/here"
    "{+x,hello,y}" =? "1024,Hello%20World!,768"
    "{+path,x}/here" =? "/foo/bar,1024/here"
    "{+path:6}/here" =? "/foo/b/here"
    "{+list}" =? "red,green,blue"
    "{+list*}" =? "red,green,blue"
    "{+keys}" =? "semi,;,dot,.,comma,,"
    "{+keys*}" =? "semi=;,dot=.,comma=,"

// Note - this may seem incorrect, but is actually expected behaviour.
// These are not ideal operators to use for matching complex data!

[<Fact>]
let ``Reserved Matching Matches Correctly`` () =
    matches "/test/{+atom}" "/test/one!" [ Key "atom", Atom "one!" ]
    matches "/test/{+atom}" "/test//one!" [ Key "atom", Atom "/one!" ]
    matches "/test/{+atom}" "/test/" [ Key "atom", Atom "" ]
    matches "/test/{+list*}" "/test/one,two,three" [ Key "list", List [ "one,two,three" ] ]
    matches "/test/{+keys*}" "/test/one=a,two=b" [ Key "keys", List [ "one=a,two=b" ] ]

[<Fact>]
let ``Fragment Expansion Renders Correctly`` () =
    "{#var}" =? "#value"
    "{#hello}" =? "#Hello%20World!"
    "{#half}" =? "#50%25"
    "foo{#empty}" =? "foo#"
    "foo{#undef}" =? "foo"
    "{#x,hello,y}" =? "#1024,Hello%20World!,768"
    "{#path,x}/here" =? "#/foo/bar,1024/here"
    "{#path:6}/here" =? "#/foo/b/here"
    "{#list}" =? "#red,green,blue"
    "{#list*}" =? "#red,green,blue"
    "{#keys}" =? "#semi,;,dot,.,comma,,"
    "{#keys*}" =? "#semi=;,dot=.,comma=,"

// Note - this may seem incorrect, but is actually expected behaviour.
// These are not ideal operators to use for matching complex data!

[<Fact>]
let ``Fragment Matching Matches Correctly`` () =
    matches "/test{#atom}" "/test#one!" [ Key "atom", Atom "one!" ]
    matches "/test{#atom}" "/test#" [ Key "atom", Atom "" ]
    matches "/test{#atom}" "/test" []
    matches "/test{#list*}" "/test#one,two,three" [ Key "list", List [ "one,two,three" ] ]
    matches "/test{#list*}" "/test#" [ Key "list", List [ "" ] ]
    matches "/test{#list*}" "/test" []
    matches "/test{#keys*}" "/test#one=a,two=b" [ Key "keys", List [ "one=a,two=b" ] ]

[<Fact>]
let ``Label Expansion with Dot-Prefix Renders Correctly`` () =
    "{.who}" =? ".fred"
    "{.who,who}" =? ".fred.fred"
    "{.half,who}" =? ".50%25.fred"
    "www{.dom*}" =? "www.example.com"
    "X{.var}" =? "X.value"
    "X{.empty}" =? "X."
    "X{.undef}" =? "X"
    "X{.var:3}" =? "X.val"
    "X{.list}" =? "X.red,green,blue"
    "X{.list*}" =? "X.red.green.blue"
    "X{.keys}" =? "X.semi,%3B,dot,.,comma,%2C"
    "X{.keys*}" =? "X.semi=%3B.dot=..comma=%2C"
    "X{.empty_keys}" =? "X"
    "X{.empty_keys*}" =? "X"

// Note - this may seem incorrect, but is actually expected behaviour.
// These are not ideal operators to use for matching complex data!

[<Fact>]
let ``Label Matching Matches Correctly`` () =
    matches "/test{.atom}" "/test.one%21" [ Key "atom", Atom "one!" ]
    matches "/test{.atom}" "/test." [ Key "atom", Atom "" ]
    matches "/test{.atom}" "/test" []
    matches "/test{.list*}" "/test.one.two.three" [ Key "list", List [ "one.two.three" ] ]
    matches "/test{.list*}" "/test." [ Key "list", List [ "" ] ]
    matches "/test{.list*}" "/test" []
    matches "/test{.keys*}" "/test.one=a.two=b" [ Key "keys", Keys [ "one", "a.two" ] ]

[<Fact>]
let ``Path Segment Expansion Renders Correctly`` () =
    "{/who}" =? "/fred"
    "{/who,who}" =? "/fred/fred"
    "{/half,who}" =? "/50%25/fred"
    "{/who,dub}" =? "/fred/me%2Ftoo"
    "{/var}" =? "/value"
    "{/var,empty}" =? "/value/"
    "{/var,undef}" =? "/value"
    "{/var,x}/here" =? "/value/1024/here"
    "{/var:1,var}" =? "/v/value"
    "{/list}" =? "/red,green,blue"
    "{/list*}" =? "/red/green/blue"
    "{/list*,path:4}" =? "/red/green/blue/%2Ffoo"
    "{/keys}" =? "/semi,%3B,dot,.,comma,%2C"
    "{/keys*}" =? "/semi=%3B/dot=./comma=%2C"

[<Fact>]
let ``Path Segment Matching Matches Correctly`` () =
    matches "/test{/atom}" "/test/one%21" [ Key "atom", Atom "one!" ]
    matches "/test{/atom}" "/test/" [ Key "atom", Atom "" ]
    matches "/test{/atom}" "/test" []
    matches "/test{/list*}" "/test/one/two/three" [ Key "list", List [ "one"; "two"; "three" ] ]
    matches "/test{/list*}" "/test/" [ Key "list", List [ "" ] ]
    matches "/test{/list*}" "/test" []
    matches "/test{/keys*}" "/test/one=a/two=b" [ Key "keys", Keys [ "one", "a"; "two", "b" ] ]

[<Fact>]
let ``Parameter Expansion Renders Correctly`` () =
    "{;who}" =? ";who=fred"
    "{;half}" =? ";half=50%25"
    "{;empty}" =? ";empty"
    "{;v,empty,who}" =? ";v=6;empty;who=fred"
    "{;v,bar,who}" =? ";v=6;who=fred"
    "{;x,y}" =? ";x=1024;y=768"
    "{;x,y,empty}" =? ";x=1024;y=768;empty"
    "{;x,y,undef}" =? ";x=1024;y=768"
    "{;hello:5}" =? ";hello=Hello"
    "{;list}" =? ";list=red,green,blue"
    "{;list*}" =? ";list=red;list=green;list=blue"
    "{;keys}" =? ";keys=semi,%3B,dot,.,comma,%2C"
    "{;keys*}" =? ";semi=%3B;dot=.;comma=%2C"

[<Fact>]
let ``Parameter Matching Matches Correctly`` () =
    matches "/test{;atom}" "/test;one" [ Key "atom", Atom "one" ]
    matches "/test{;atom}" "/test;" [ Key "atom", Atom "" ]
    matches "/test{;atom}" "/test" []
    matches "/test{;list*}" "/test;" [ Key "list", List [ "" ] ]
    matches "/test{;keys*}" "/test;one=two" [ Key "keys", Keys [ "one", "two" ] ]

[<Fact>]
let ``Query Expansion Renders Correctly`` () =
    "{?who}" =? "?who=fred"
    "{?half}" =? "?half=50%25"
    "{?x,y}" =? "?x=1024&y=768"
    "{?x,y,empty}" =? "?x=1024&y=768&empty="
    "{?x,y,undef}" =? "?x=1024&y=768"
    "{?var:3}" =? "?var=val"
    "{?list}" =? "?list=red,green,blue"
    "{?list*}" =? "?list=red&list=green&list=blue"
    "{?keys}" =? "?keys=semi,%3B,dot,.,comma,%2C"
    "{?keys*}" =? "?semi=%3B&dot=.&comma=%2C"

[<Fact>]
let ``Query Expansion Matches Correctly`` () =
    matches "/test{?who}" "/test?who=fred" [ Key "who", Atom "fred" ]
    matches "/test{?x,y}" "/test?x=1024&y=768" [ Key "x", Atom "1024"; Key "y", Atom "768" ]
    matches "/test{?x,y,empty}" "/test?x=1024&y=768&empty=" [ Key "x", Atom "1024"; Key "y", Atom "768"; Key "empty", Atom "" ]
    matches "/test{?list}" "/test?list=red,green,blue" [ Key "list", List [ "red"; "green"; "blue" ] ]
    matches "/test{?list*}" "/test?list=red&list=green&list=blue" [ Key "list", List [ "red"; "green"; "blue" ] ]
    matches "/test{?keys*}" "/test?semi=%3B&dot=.&comma=%2C" [ Key "keys", Keys [ ("semi", ";"); ("dot", "."); ("comma", ",") ] ]
    matches "/test{?who}" "/test" []
    matches "/test{?who}" "/test?" []
    matches "/test{?who}" "/test?&" []
    matches "/test{?who}" "/test?who" [ Key "who", Atom "" ]
    matches "/test{?who}" "/test?who&" [ Key "who", Atom "" ]
    matches "/test{?who}" "/test?who=" [ Key "who", Atom "" ]
    matches "/test{?who}" "/test?who=&" [ Key "who", Atom "" ]
    matches "/test{?list*}" "/test" []
    matches "/test{?list*}" "/test?" []
    matches "/test{?list*}" "/test?&" []
    matches "/test{?list*}" "/test?list" [ Key "list", List [""] ]
    //matches "/test{?list*}" "/test?list&" [ Key "list", List [""] ]
    matches "/test{?list*}" "/test?list=" [ Key "list", List [""] ]
    //matches "/test{?list*}" "/test?list=&" [ Key "list", List [""] ]
    matches "/test{?list*}" "/test?list=&list" [ Key "list", List [ ""; "" ] ]

[<Fact>]
let ``Query Continuation Expansion Renders Correctly`` () =
    "{&who}" =? "&who=fred"
    "{&half}" =? "&half=50%25"
    "?fixed=yes{&x}" =? "?fixed=yes&x=1024"
    "{&x,y,empty}" =? "&x=1024&y=768&empty="
    "{&x,y,undef}" =? "&x=1024&y=768"
    "{&var:3}" =? "&var=val"
    "{&list}" =? "&list=red,green,blue"
    "{&list*}" =? "&list=red&list=green&list=blue"
    "{&keys}" =? "&keys=semi,%3B,dot,.,comma,%2C"
    "{&keys*}" =? "&semi=%3B&dot=.&comma=%2C"

[<Fact>]
let ``Query Continuation Expansion Matches Correctly`` () =
    matches "/test?fixed{&who}" "/test?fixed&who=fred" [ Key "who", Atom "fred" ]
    matches "/test?fixed{&x,y}" "/test?fixed&x=1024&y=768" [ Key "x", Atom "1024"; Key "y", Atom "768" ]
    matches "/test?fixed{&x,y,empty}" "/test?fixed&x=1024&y=768&empty=" [ Key "x", Atom "1024"; Key "y", Atom "768"; Key "empty", Atom "" ]
    matches "/test?fixed{&list}" "/test?fixed&list=red,green,blue" [ Key "list", List [ "red"; "green"; "blue" ] ]
    matches "/test?fixed{&list*}" "/test?fixed&list=red&list=green&list=blue" [ Key "list", List [ "red"; "green"; "blue" ] ]
    matches "/test?fixed{&keys*}" "/test?fixed&semi=%3B&dot=.&comma=%2C" [ Key "keys", Keys [ ("semi", ";"); ("dot", "."); ("comma", ",") ] ]
    matches "/test?fixed{&who}" "/test?fixed" []
    matches "/test?fixed{&who}" "/test?fixed&" []
    matches "/test?fixed{&who}" "/test?fixed&&" []
    matches "/test?fixed{&who}" "/test?fixed&who" [ Key "who", Atom "" ]
    matches "/test?fixed{&who}" "/test?fixed&who&" [ Key "who", Atom "" ]
    matches "/test?fixed{&who}" "/test?fixed&who=" [ Key "who", Atom "" ]
    matches "/test?fixed{&who}" "/test?fixed&who=&" [ Key "who", Atom "" ]
    matches "/test?fixed{&list*}" "/test?fixed" []
    matches "/test?fixed{&list*}" "/test?fixed&" []
    matches "/test?fixed{&list*}" "/test?fixed&&" []
    matches "/test?fixed{&list*}" "/test?fixed&list" [ Key "list", List [""] ]
    //matches "/test?fixed{&list*}" "/test?fixed&list&" [ Key "list", List [""] ]
    matches "/test?fixed{&list*}" "/test?fixed&list=" [ Key "list", List [""] ]
    //matches "/test?fixed{&list*}" "/test?fixed&list=&" [ Key "list", List [""] ]
    matches "/test?fixed{&list*}" "/test?fixed&list=&list" [ Key "list", List [ ""; "" ] ]
