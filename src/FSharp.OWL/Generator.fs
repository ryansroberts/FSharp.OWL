module Generator

open ProviderImplementation.ProvidedTypes
open Schema
open FsRdf
open Prefixes
open System.Reflection
open Microsoft.FSharp.Quotations
open VDS.RDF
open System.Linq
open System.Linq.Expressions
open Store

type OntologyNode(uri : string) = 
    member x.Uri = Node.Uri(Schema.Uri uri)

type Class(uri) = 
    inherit OntologyNode(uri)

type Individual(uri) = 
    inherit OntologyNode(uri)

type ObjectProperty(uri) = 
    inherit OntologyNode(uri)

type DataProperty(uri) = 
    inherit OntologyNode(uri)

let ignoreBlank nx = 
    nx |> Seq.filter (function 
              | Node.Uri(Blank(b)) -> false
              | _ -> true)

let typeName (ns : prefixes) (uri : Schema.Uri) = 
    let uri = 
        match uri with
        | Schema.Uri(uri) -> uri
    
    let uri = System.Uri uri
    let matchingPrefix = List.tryFind (fun (p, u) -> uri.ToString().StartsWith(string u))
    match uri.Fragment with
    | fragment when not (System.String.IsNullOrEmpty fragment) -> 
        match ns |> matchingPrefix with
        | Some(p, u) -> (sprintf "%s:%s" p (fragment.Substring(1)))
        | None -> string uri
    | _ -> 
        match ns |> matchingPrefix with
        | Some(p, u) -> (sprintf "%s:%s" p uri.Segments.[uri.Segments.Length])
        | None -> string uri

let className (ns : prefixes) (cls : Schema.Class) = 
    let uris = 
        cls.EquivalentClasses
        |> Seq.map (typeName ns)
        |> List.ofSeq
    if uris.Length = 0 then typeName ns cls.Uri
    else sprintf "%s≡%s" (typeName ns cls.Uri) (uris |> String.concat "≡")

let vdsUri (g : IGraph) (u : Schema.Uri) = 
    match u with
    | Uri.Uri(s) -> g.CreateUriNode(System.Uri s)
    | Uri.QName(p, n) -> g.CreateUriNode(System.Uri(p + n))

let rec classNode ns uri ont = 
    let cs = ont (string uri)
    printf "%A" cs
    let cls = ProvidedTypeDefinition(className ns cs, Some typeof<OntologyNode>)
    let ctor = ProvidedConstructor([])
    let ctorInfo = 
        typeof<Class>.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ <@@ (uri) @@> ]
    ctor.InvokeCode <- fun args -> <@@ () @@>
    cls.AddMember ctor
    let sc = ProvidedTypeDefinition("SubClasses", Some typeof<obj>)
    cls.AddMember sc
    (fun () -> 
    [ for sub in cs.Subtypes do
          yield classNode ns sub ont ])
    |> sc.AddMembersDelayed
    let op = ProvidedTypeDefinition("ObjectProperties", Some typeof<obj>)
    cls.AddMember op
    (fun () -> 
    [ for (p, r) in cs.ObjectProperties do
          yield objectPropertyType ns p r ont ])
    |> op.AddMembersDelayed
    let op = ProvidedTypeDefinition("DataProperties", Some typeof<obj>)
    cls.AddMember op
    (fun () -> 
    [ for (p, r) in cs.DataProperties do
          yield dataPropertyType ns p r ont ])
    |> op.AddMembersDelayed
    (fun () -> 
    [ for (p, r) in cs.ObjectProperties do
          yield objectProperty ns p r ont ])
    |> cls.AddMembersDelayed
    cls

and objectProperty ns p r ont = 
    let prop = ProvidedProperty(typeName ns p, typeof<Schema.Uri>)
    prop.GetterCode <- fun args -> <@@ p @@>
    prop

and objectPropertyType ns p r ont = ProvidedTypeDefinition(typeName ns p, Some typeof<obj>)

and dataPropertyType ns p r ont = ProvidedTypeDefinition(typeName ns p, Some typeof<obj>)

let root (t : ProvidedTypeDefinition) ns root ont = 
    t.AddMember(classNode ns root ont)
    t
