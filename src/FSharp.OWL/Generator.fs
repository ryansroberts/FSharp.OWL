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
open Gubbins

let (|Flags|_|) f input = 
    if ((List.reduce (|||) f) &&& input <> Characteristics.None) then Some f
    else None

type GenerationContext = 
    { ns : prefixes
      uri : Uri
      ont : string -> Class }

type OntologyNode(uri : string) = 
    member x.Uri = Schema.Uri uri

type Class(uri) = 
    inherit OntologyNode(uri)

type Individual(uri) = 
    inherit OntologyNode(uri)

type ObjectProperty(uri) = 
    inherit OntologyNode(uri)

type DataProperty(uri) = 
    inherit OntologyNode(uri)

type Restriction(uri) = 
    inherit OntologyNode(uri)

let ignoreBlank nx = 
    nx |> Seq.filter (function 
              | Node.Uri(Blank(b)) -> false
              | _ -> true)

let typeName (ns : prefixes) (uri : Uri) = 
    let uri = 
        match uri with
        | Schema.Uri(uri) -> uri
    
    let uri = System.Uri uri
    let matchingPrefix = List.tryFind (fun (p, u) -> uri.ToString().StartsWith(string u))
    match uri.Fragment, ns |> matchingPrefix with
    | NotEmpty fragment, Some("base", _) -> fragment.Substring(1)
    | NotEmpty fragment, Some(prefix, _) -> sprintf "%s:%s" prefix (fragment.Substring(1))
    | fragment, Some("base", _) -> uri.Segments.[uri.Segments.Length]
    | fragment, Some(prefix, _) -> sprintf "%s:%s" prefix (uri.Segments.[uri.Segments.Length])
    | _, _ -> string uri

let className (ns : prefixes) (cls : Schema.Class) = 
    let uris = 
        cls.EquivalentClasses
        |> Seq.map (typeName ns)
        |> List.ofSeq
    if uris.Length = 0 then typeName ns cls.Uri
    else sprintf "%s≡%s" (typeName ns cls.Uri) (uris |> String.concat "≡")

let restrictionName (ns : prefixes) (range : Set<Uri>) = 
    range
    |> Set.toArray
    |> Array.map (fun u -> (string u))
    |> String.concat " . "

let vdsUri (g : IGraph) (u : Schema.Uri) = 
    match u with
    | Uri.Uri(s) -> g.CreateUriNode(System.Uri s)
    | Uri.QName(p, n) -> g.CreateUriNode(System.Uri(p + n))

let objectProperty (ctx : GenerationContext) (ch : Characteristics) (p : Schema.PropertyRange) = 
    let scalar pn = 
        let restriction = ProvidedTypeDefinition(restrictionName ctx.ns pn.Range, Some typeof<obj>)
        let ctor = restriction.GetConstructors() |> Seq.exactlyOne
        let n = typeName ctx.ns ctx.uri
        let field = ProvidedField("_" + n, restriction)
        field.SetFieldAttributes(FieldAttributes.Private)
        let prop = ProvidedProperty(n, restriction, GetterCode = (fun [ this ] -> <@@ null @@>))
        (prop, field, restriction)
    
    let collection pn = 
        let restriction = ProvidedTypeDefinition(restrictionName ctx.ns pn.Range, Some typeof<obj>)
        let lt = typedefof<List<_>>.MakeGenericType([| restriction :> System.Type |])
        let ctor = restriction.GetConstructors() |> Seq.exactlyOne
        let n = typeName ctx.ns ctx.uri
        let field = ProvidedField("_" + n, lt)
        field.SetFieldAttributes(FieldAttributes.Private)
        let prop = ProvidedProperty(n, lt, GetterCode = (fun [ this ] -> <@@ null @@>))
        (prop, field, restriction)
    
    match ch with
    | Characteristics.Functional _ -> scalar p
    | Flags [Characteristics.InverseFunctional ] _ -> scalar p
    | _ -> collection p

let individualType (ctx : GenerationContext) cs (rx : ProvidedProperty list) = 
    let t = ProvidedTypeDefinition("Individual", Some typeof<Individual>)
    
    let ctor = 
        ProvidedConstructor
            (ProvidedParameter("self", typeof<Schema.Uri>) 
             :: [ for r in rx -> ProvidedParameter(r.Name, typeof<Schema.Uri>) ])
    
    let ctorInfo = 
        typeof<Individual>
            .GetConstructor(BindingFlags.Public ||| BindingFlags.Instance, null, [| typeof<string> |], null)
    ctor.BaseConstructorCall <- fun args -> ctorInfo, [ args.[0] ]
    ctor.InvokeCode <- fun args -> <@@ args @@>
    (fun () -> rx) |> t.AddMembersDelayed
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
        [ for uri, ch, p in cs.ObjectProperties do
              yield objectPropertyType { ctx with uri = uri } p ])
        |> op.AddMembersDelayed
    if cs.DataProperties.Any() then 
        let op = ProvidedTypeDefinition("DataProperties", Some typeof<obj>)
        cls.AddMember op
        (fun () -> 
        [ for (p, r) in cs.DataProperties do
              yield dataPropertyType { ctx with uri = p } r ])
        |> op.AddMembersDelayed
    let op = ProvidedTypeDefinition("Restrictions", Some typeof<obj>)
    cls.AddMember op
    (fun () -> 
    [ let rx = 
          [ for uri, ch, p in cs.ObjectProperties do
                let (prop, field, restriction) = objectProperty { ctx with uri = uri } ch p
                op.AddMember restriction
                yield (prop, field) ]
      
      let (ctor, individualType) = individualType ctx cs (rx |> List.map fst)
      let individuals = 
          ProvidedMethod
              ("Individuals", [ ProvidedParameter("store", typeof<Store.store>) ], 
               typedefof<IQueryable<_>>.MakeGenericType([| individualType :> System.Type |]), 
               InvokeCode = (fun args -> <@@ () @@>), IsStaticMethod = true)
      for p, f in rx do
          yield p :> MemberInfo
          yield f :> MemberInfo
      yield individuals :> MemberInfo
      yield individualType :> MemberInfo ])
    |> cls.AddMembersDelayed
    cls

and objectPropertyType (ctx : GenerationContext) r = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<obj>)

and dataPropertyType (ctx : GenerationContext) r = ProvidedTypeDefinition(typeName ctx.ns ctx.uri, Some typeof<obj>)

let root (t : ProvidedTypeDefinition) ns root ont = 
    let cls = 
        classNode { ns = ns
                    uri = root
                    ont = ont }
    t.AddMembers [ cls ]
    t.AddMember(ProvidedMethod("root", [], cls, InvokeCode = (fun a -> <@@ cls @@>)))
    t
