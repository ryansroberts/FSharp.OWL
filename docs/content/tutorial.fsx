(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Introducing your project
========================

Say more

*)
#r "FSharp.OWL"

[<Literal>]
let ttl = __SOURCE_DIRECTORY__ + "../../../tests/FSharp.OWL.Tests/wine.ttl"

type wine = LinkedData.Memory<ttl,
    "http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine",
    """wine:http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine,
       owl:http://www.w3.org/2002/07/owl,
       food:http://www.w3.org/TR/2003/PR-owl-guide-20031209/food""">

type thing = LinkedData.Memory<ttl,
    "http://www.w3.org/2002/07/owl#thing",
    """wine:http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine,
       owl:http://www.w3.org/2002/07/owl,
       food:http://www.w3.org/TR/2003/PR-owl-guide-20031209/food""">

let aWine = wine.``wine:Wine≡food:Wine`` ()


let uri = aWine.Uri

(**
Some more info
*)
