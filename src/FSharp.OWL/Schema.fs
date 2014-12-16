module Schema 
open VDS.RDF

type Uri =
  | Uri of string
  | QName of string * string
  | Blank of string
with static member fromString (p,n) = QName (p,n)
     static member fromString u = Uri u 
     override x.ToString () =
       match x with
         | Uri u -> u
         | QName (p,n) -> p + n

let (++) p n  = QName(p,n)

type Literal =
  | Literal of string

type Node =
  | Uri of Uri
  | Literal of Literal
with static member fromVDS (n:INode) =
  match n with
    | :? IUriNode as n -> Node.Uri (Uri.Uri (string n.Uri))
    | :? ILiteralNode as n -> Node.Literal ( Literal.Literal(string n))
    | :? IBlankNode as n -> Node.Uri(Uri.Blank(n.InternalID))
    | _ -> failwith (sprintf "Unknown node %A" (n.GetType ()))
    
type Subject =
| Subject of Uri

type Predicate =
| Predicate of Uri

type Object =
| Object of Node

type Binding =
| Binding of string
| Wildcard

type QueryType =
| Select of Binding list

type QueryPattern =
| Binding of Binding
| OfType 
| Node of Node  
with static member uri u = QueryPattern.Node(u)
     static member var v = QueryPattern.Binding(Binding.Binding v)
     
type BGP =
| BGP of QueryPattern * QueryPattern * QueryPattern
with static member a b t = BGP(QueryPattern.var b,QueryPattern.OfType,QueryPattern.uri t)
     static member anIndividual b = BGP.a b (Node.Uri(Uri.Uri("http://www.w3.org/2002/07/owl#Individual")))
     static member anyStatement s p o = BGP(QueryPattern.Binding(Binding.Binding s),
                                            QueryPattern.Binding(Binding.Binding p),
                                            QueryPattern.Binding(Binding.Binding o))
type Where=
| Where of BGP list

type Sparql = QueryType * Where

open VDS.RDF.Query
open VDS.RDF.Parsing
open VDS.RDF.Query.Datasets

type Cardinality =
| Unspecified
| Constrained of int * int

type PropertyRange = {
  Range : Set<Uri>
  Cardinality : Cardinality
}

[<System.Flags>]
type Characteristics = 
| None              = 0b000000000
| Functional        = 0b000000001
| InverseFunctional = 0b000000010
| Transitive        = 0b000000100
| Symmetric         = 0b000010000
| Asymmetric        = 0b000100000
| Reflexive         = 0b001000000
| Irreflexive       = 0b010000000

type Property = Uri *  PropertyRange  

type ClassExpression =
| Union of ClassExpression
| HasValue of Uri
| SomeValuesFrom of Uri

type ResultGraph (g:IGraph) =
  let prefix q = """prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            prefix owl:  <http://www.w3.org/2002/07/owl#>
            prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            """ + q
  
  let projectNode (p:LeviathanQueryProcessor) f q =
    p.ProcessQuery ((new SparqlQueryParser ()).ParseFromString (prefix q))
    :?> SparqlResultSet
    |> Seq.map f 
    |> Set.ofSeq
 
  member private x.p =  LeviathanQueryProcessor (InMemoryDataset (g))
  member x.One (q:string) = projectNode x.p (fun i -> i.[0] |> Node.fromVDS) q
  member x.Two (q:string) = projectNode x.p (fun i -> (i.[0] |> Node.fromVDS ,
                                                    i.[1] |> Node.fromVDS)) q
  member x.Three (q:string) = projectNode x.p (fun i -> (i.[0] |> Node.fromVDS,
                                                      i.[1] |> Node.fromVDS,
                                                      i.[2] |> Node.fromVDS)) q 

let nonBlank s = s |> Seq.filter (function | Node.Uri(Blank(n)) -> true | _ -> false) |> Set.ofSeq

type ObjectProperty = Property

type DataProperty = Property

type Class = {
    Uri : Uri 
    Label : Set<Literal> 
    ObjectProperties : Set<(Uri * Characteristics * PropertyRange)>
    DataProperties : Set<(Uri * Set<Uri>)>
    Subtypes : Set<Uri>
    Supertypes : Set<Uri>
    EquivalentClasses : Set<Uri>
    }

open Gubbins

let defaultNs = 
      [ ("owl",  Uri.fromString "http://www.w3.org/2002/07/owl#")
        ("rdfs", Uri.fromString "http://www.w3.org/2000/01/rdf-schema#")
        ("rdf",  Uri.fromString "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
        ("xsd",  Uri.fromString "http://www.w3.org/2001/XMLSchema#") ]

type prefixes = (string * Uri) list
let parse (s : string) = 
      [ for l in s.Split ',' do
            match l with
            | Regex "(\w+):(.+)" gx -> yield (gx.Head, Uri.fromString (gx.Tail.Head))
            | _ -> () ]


