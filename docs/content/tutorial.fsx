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

let wineStore = Store.loadFile ttl

[<Literal>]
let xml = __SOURCE_DIRECTORY__ + "../../../tests/FSharp.OWL.Tests/pizza.xml"

[<Literal>]
let hydraTtl = __SOURCE_DIRECTORY__ + "../../../tests/FSharp.OWL.Tests/core.ttl"

type hydraOntology = LinkedData.Memory<ttl,
    "http://www.w3.org/ns/hydra/core#Resource",
    """hydra:http://www.w3.org/ns/hydra/core#""">

type wineOntology = LinkedData.Memory<ttl,
    "http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine",
    """wine:http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine,
       owl:http://www.w3.org/2002/07/owl,
       food:http://www.w3.org/TR/2003/PR-owl-guide-20031209/food""">

type pizzaOntology = LinkedData.Memory<xml,
                     "http://www.co-ode.org/ontologies/pizza/pizza.owl#Pizza",
                     """pizza:http://www.co-ode.org/ontologies/pizza/pizza.owl#,
                        owl:http://www.w3.org/2002/07/owl""">

let wines = wineOntology.``wine:Wine``.Individuals wineStore

let redWines = query {
    for wine in wines do
    select wine   
}

(**
Some more info
*)
