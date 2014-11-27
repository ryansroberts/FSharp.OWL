module Generator

open ProviderImplementation.ProvidedTypes
open Schema
open Prefixes
open System.Reflection
open Microsoft.FSharp.Quotations
open VDS.RDF
open System.Linq
open System.Linq.Expressions
open Store
open OWLQueryable

type GenerationContext = 
    { ns : prefixes
      uri : Uri
      ont : string -> Class }

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

let restrictionName (ns : prefixes) (range : Set<Uri>) = 
    if range.Count = 1 then typeName ns (range |> Seq.head)
    else 
        range
        |> Set.toArray
        |> Array.map (fun u -> (string u))
        |> String.concat " . "

let vdsUri (g : IGraph) (u : Schema.Uri) = 
    match u with
    | Uri.Uri(s) -> g.CreateUriNode(System.Uri s)
    | Uri.QName(p, n) -> g.CreateUriNode(System.Uri(p + n))

let individualParams ctx cs = 
    cs.ObjectProperties
    |> Seq.map (fun (p, r) -> ProvidedParameter(typeName ctx.ns p, typeof<Uri>))
    |> Seq.toList

let individualType (ctx : GenerationContext) cs  = 
    let t = ProvidedTypeDefinition((typeName ctx.ns ctx.uri), Some typeof<Individual>)
    let ctor = ProvidedConstructor(individualParams ctx cs)
    let ctorInfo = 
        typeof<Individual>
            .GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ <@@ args.[0] @@> ]
    ctor.InvokeCode <- fun args -> <@@ args.[0] @@>
    t.AddMember ctor
    (ctor, t)

let rec classNode (ctx : GenerationContext) = 
    let cs = ctx.ont (string ctx.uri)
    let cls = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<OntologyNode>)
    cls.AddXmlDoc(sprintf """
        <summary> 
            Equivalents: %s    
        </summary>
    """ (className ctx.ns cs))
    let ctor = ProvidedConstructor([])
    let ctorInfo = 
        typeof<Class>.GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ <@@ (ctx.uri) @@> ]
    ctor.InvokeCode <- fun args -> <@@ () @@>
    cls.AddMember ctor
    if cs.Subtypes.Any() then 
        let sc = ProvidedTypeDefinition("SubClasses", Some typeof<obj>)
        cls.AddMember sc
        (fun () -> 
        [ for sub in cs.Subtypes do
              yield classNode { ctx with uri = sub } ])
        |> sc.AddMembersDelayed
    if cs.ObjectProperties.Any() then 
        let op = ProvidedTypeDefinition("ObjectProperties", Some typeof<obj>)
        cls.AddMember op
        (fun () -> 
        [ for (p, r) in cs.ObjectProperties do
              yield objectPropertyType { ctx with uri = p } r ])
        |> op.AddMembersDelayed
    if cs.DataProperties.Any() then 
        let op = ProvidedTypeDefinition("DataProperties", Some typeof<obj>)
        cls.AddMember op
        (fun () -> 
        [ for (p, r) in cs.DataProperties do
              yield dataPropertyType { ctx with uri = p } r ])
        |> op.AddMembersDelayed
    if cs.ObjectProperties.Any() then 
        let op = ProvidedTypeDefinition("Restrictions", Some typeof<obj>)
        cls.AddMember op
        (fun () -> 
        [ for (p, r) in cs.ObjectProperties do
              let (prop, restriction) = objectProperty { ctx with uri = p } r
              op.AddMember restriction
              yield prop :> MemberInfo ])
        |> cls.AddMembersDelayed

    let (ctor, individualType) = individualType ctx cs
    let individuals = ProvidedMethod("Individuals",
        [ProvidedParameter("store",typeof<Store.store>)],
        typedefof<IQueryable<_>>.MakeGenericType([|individualType :> System.Type|]),
        InvokeCode = (fun args -> <@@ () @@>),IsStaticMethod = true
        )
    
    cls.AddMember individuals  

    let createIndividual = 
        ProvidedMethod
            ("CreateIndividual", individualParams ctx cs, individualType, 
             InvokeCode = (fun args -> <@@ Expr.NewObject(ctor, args) @@>),IsStaticMethod = true)
    cls.AddMember individualType
    cls.AddMember createIndividual
    cls

and objectProperty (ctx : GenerationContext) r = 
    let restriction = ProvidedTypeDefinition(restrictionName ctx.ns r, Some typeof<obj>)
    let ctor = restriction.GetConstructors() |> Seq.exactlyOne
    let prop = ProvidedProperty(typeName ctx.ns ctx.uri, restriction)
    prop.GetterCode <- fun args -> <@@ Expr.NewObject(ctor, []) @@>
    (prop, restriction)

and objectPropertyType (ctx : GenerationContext) r = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<obj>)

and dataPropertyType (ctx : GenerationContext) r = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<obj>)



let root (t : ProvidedTypeDefinition) ns root ont = 
    let cls = classNode {ns=ns;uri=root;ont=ont}
    t.AddMember cls
    t.AddMember(ProvidedMethod("root", [], cls, 
                               InvokeCode = (fun a -> <@@ cls @@>)))
    t
