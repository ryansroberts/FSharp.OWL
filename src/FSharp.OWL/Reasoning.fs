module Reasoning

open Schema
open org.semanticweb.owlapi.apibinding
open org.semanticweb.owlapi.model
open org.semanticweb.owlapi.util
open org.semanticweb.owlapi.io
open org.coode.owlapi.manchesterowlsyntax
open org.semanticweb.owlapi.vocab
open System.Collections.Generic
open org.semanticweb.owlapi.reasoner
open org.semanticweb.owlapi.reasoner.structural
open uk.ac.manchester.cs.owlapi.modularity
open uk.ac.manchester.cs.owl.owlapi
open org.semanticweb.owlapi.apibinding
open org.semanticweb.owlapi.model
open org.semanticweb.owlapi.reasoner
open Microsoft.FSharp.Linq.RuntimeHelpers

type Ontology = 
    | Ontology of OWLOntology

type Reasoner = 
    | Reasoner of OWLReasoner

type DataFactory = 
    | Factory of OWLDataFactory

type Uri with
    static member fromIRI (iri : IRI) = Uri.Uri(iri.toString())
    static member fromHasUri (has : HasIRI) = Uri.Uri((has.getIRI()).toString())

let rec iter<'a> (nx : java.util.Set) = 
    match nx with
    | :? NodeSet as nx -> iter<'a> (nx.getFlattened())
    | _ -> 
        [ let i = nx.iterator()
          while i.hasNext() do
              yield i.next() :?> 'a ]

let rec splitIntersections (c : obj) = 
    [ match c with
      | :? OWLObjectIntersectionOf as x -> 
          yield! x.getClassesInSignature()
                 |> iter<OWLEntity>
                 |> List.map splitIntersections
                 |> List.concat
      | :? HasIRI as x -> yield Uri.fromHasUri x ]


let domainMap (o : OWLOntology) (r : OWLReasoner) (f : OWLDataFactory) = 
    let characteristicsOf (p:OWLEntity) = 
        let test f p = 
            if f then p
            else Characteristics.None       
        match p with 
        | :? OWLObjectProperty as p ->
            let isFunctional p = 
                f.getOWLObjectMinCardinality(2, p)
                |> r.isSatisfiable
                |> not

            test (isFunctional (p)) Characteristics.Functional 
            ||| test (p.isInverseFunctional (o)) Characteristics.InverseFunctional 
            ||| test (p.isTransitive (o)) Characteristics.Transitive 
            ||| test (p.isSymmetric (o)) Characteristics.Symmetric 
            ||| test (p.isAsymmetric (o)) Characteristics.Asymmetric 
            ||| test (p.isReflexive (o)) Characteristics.Reflexive 
            ||| test (p.isIrreflexive (o)) Characteristics.Irreflexive
        | :? OWLDataProperty as p -> test(p.isFunctional(o)) Characteristics.Functional
            
        
    let ox = [for p in o.getObjectPropertiesInSignature() |> iter<OWLObjectProperty> do
                for c in r.getObjectPropertyDomains(p,true).getFlattened() 
                         |> iter<OWLClass> do
                         
                    for c in c::(r.getSubClasses(c,false).getFlattened()
                            |> iter<OWLClass>) |> Set.ofList do
                
                        yield (c,characteristicsOf p,p)
              ]

    let dx = [for p in o.getDataPropertiesInSignature() |> iter<OWLDataProperty> do
                for c in r.getDataPropertyDomains(p,true).getFlattened() 
                         |> iter<OWLClass> do

                    for c in c::(r.getSubClasses(c,false).getFlattened()
                            |> iter<OWLClass>) |> Set.ofList do
                
                        yield (c,characteristicsOf p,p)
              ]

    let mapOf px =
        px             
        |> Seq.groupBy (fun (c, _, _) -> c)
        |> Seq.map (fun (c, p) -> (c, p |> Seq.map (fun (c, ch, p) -> (ch, p))))
        |> Map.ofSeq

    (mapOf ox,mapOf dx)

type ReasoningContext = 
    { Ontology : OWLOntology
      Reasoner : OWLReasoner
      DataFactory : OWLDataFactory
      ObjectDomain : Map<OWLClass, (Characteristics * OWLObjectProperty) seq>
      DataDomain : Map<OWLClass, (Characteristics * OWLDataProperty) seq> }
    static member create (o, r, f) = 
        match o, r, f with
        | Ontology o, Reasoner r, Factory f -> 
            let (oprops, dataprops) = domainMap o r f
            { Ontology = o
              Reasoner = r
              DataFactory = f
              ObjectDomain = oprops
              DataDomain = dataprops }

let inline (>>>) x p =  
    printfn p x
    x
(*
  Cribbed from Protege's RestrictedPropertyExtractor
*)
type propertyExtractor () =
    let mutable extracted : Set<Constraint> = Set.empty
    let extract (node:OWLQuantifiedRestriction) = 
     extracted <-  
            extracted.Add (Constraint.SomeOf( 
                node.getFiller().getObjectPropertiesInSignature()
                |> iter<OWLEntity> 
                |> List.map Uri.fromHasUri
                |> Set.ofList))
     ()
    member x.Extracted() = extracted
    interface OWLClassExpressionVisitor with
        member x.visit(node:OWLClass) =
            node.getClassesInSignature()
            |> iter<OWLClass>
            |> List.iter (fun e -> e.accept(x))
            ()
        member x.visit(node:OWLObjectIntersectionOf) =
            node.getOperands()
            |> iter<OWLClass>
            |> List.iter (fun e -> e.accept(x))
            ()       
        member x.visit (node:OWLObjectComplementOf) =
            node.getOperand().accept(x)
        member x.visit (node:OWLObjectUnionOf) =
            node.getOperands()
                |> iter<OWLClass>
                |> List.iter (fun e -> e.accept(x))
            ()
        member x.visit (node:OWLDataAllValuesFrom) = extract node 
        member x.visit (node:OWLDataSomeValuesFrom) = extract node 
        member x.visit (node:OWLDataHasValue) = ()
        member x.visit (node:OWLObjectAllValuesFrom) = extract node
        member x.visit (node:OWLObjectSomeValuesFrom) = extract node
        member x.visit (node:OWLObjectHasValue) = ()
        member x.visit (node:OWLObjectMinCardinality) = extract node
        member x.visit (node:OWLObjectExactCardinality) = extract node
        member x.visit (node:OWLObjectMaxCardinality) = extract node
        member x.visit (node:OWLObjectHasSelf) = ()
        member x.visit (node:OWLObjectOneOf) = ()
        member x.visit (node:OWLDataMinCardinality) = extract node
        member x.visit (node:OWLDataExactCardinality) = extract node
        member x.visit (node:OWLDataMaxCardinality) = extract node


let extractIri ex = ex |> List.map Uri.fromHasUri

let objectProperties ctx (c : OWLClass) = 
    let px = propertyExtractor() :> OWLClassExpressionVisitor
     
    px.visit(c)
    printfn "Visited: %A" (px)
     
    let rangeOf (c:OWLClassExpression) (p:OWLObjectPropertyExpression) = [
        let common = ctx.Reasoner.getObjectPropertyRanges(p,true).getFlattened() |> iter<OWLClass>
        let possible c = 
            ctx.DataFactory.getOWLObjectSomeValuesFrom (p, c)
        for c in common do
            yield! ctx.Reasoner.getEquivalentClasses(c).getEntities() 
                    |> iter<OWLClass>
                    |> List.filter (fun c ->  ctx.Reasoner.isSatisfiable(possible c))   
            yield! ctx.Reasoner.getSubClasses(c,false).getFlattened()
                    |> iter<OWLClass>
                    |> List.rev
                    |> Seq.filter (fun c -> ctx.Reasoner.isSatisfiable(possible c))   
    ]
    
    let inClosure (cx : OWLClass list) = cx |> List.filter (fun c -> ctx.Ontology.containsEntityInSignature (c, true))
    
    let cardinality ch (p:OWLProperty) =  
        match ch with 
        | _ -> Cardinality.Unspecified

    ctx.ObjectDomain
    |> Map.find c
    |> Seq.map (fun (ch, p) -> 
           (Uri.fromHasUri p, ch, 
            { Range = 
                  rangeOf c p
                  |> inClosure
                  |> extractIri
                  |> Set.ofList
              Cardinality = cardinality ch p }))

let subTypes ctx (c : OWLClass) = 
    ctx.Reasoner.getSubClasses(c, true).getFlattened()
    |> iter<OWLClass>
    |> List.map Uri.fromHasUri

let superTypes ctx (c : OWLClass) = 
    ctx.Reasoner.getSuperClasses(c, true).getFlattened()
    |> iter<OWLClass>
    |> List.map Uri.fromHasUri

let labels ctx (e:OWLEntity) =
    e.getAnnotations(ctx.Ontology)
    |> iter<OWLAnnotation>
    |> List.filter (fun a -> a.getProperty().isLabel())
    |> List.map (fun a -> Literal.Literal (string (a.getValue())))

let comments ctx (e:OWLEntity) =
    e.getAnnotations(ctx.Ontology)
    |> iter<OWLAnnotation>
    |> List.filter (fun a -> a.getProperty().isComment())
    |> List.map (fun a -> Literal.Literal (string (a.getValue())))

type OntologyManager() = 
    member x.manager = OWLManager.createOWLOntologyManager()
    
    member x.loadFile (p : string) = 
        try 
            Ontology(x.manager.loadOntologyFromOntologyDocument (java.io.File(p))) |> x.reason
        with :? OWLOntologyAlreadyExistsException as e -> 
            Ontology(x.manager.getOntology (e.getDocumentIRI())) |> x.reason
    
    member private x.reason o = 
        match o with
        | Ontology(o) -> 
            let reasonerFactory = Cognitum.OwlApi.Net.Pellet.NetReasonerFactoryImpl()
            let config = new SimpleConfiguration()
            let reasoner = reasonerFactory.createReasoner (o, config)
            reasoner.precomputeInferences ([| InferenceType.CLASS_ASSERTIONS; InferenceType.CLASS_HIERARCHY |])
            Ontology(reasoner.getRootOntology()), Reasoner reasoner, 
            Factory(o.getOWLOntologyManager().getOWLDataFactory())
    
    member x.schema ctx (iri : string) = 
        let cs = (ctx.DataFactory.getOWLClass(IRI.create iri).asOWLClass())
        { Uri = Uri.Uri(iri)
          Label = labels ctx cs
          Comments = comments ctx cs
          ObjectProperties = objectProperties ctx cs |> Set.ofSeq
          DataProperties = Set.empty
          EquivalentClasses = 
              ctx.Reasoner.getEquivalentClasses(cs).getEntities()
              |> iter<obj>
              |> List.map splitIntersections
              |> List.concat
              |> Set.ofList
          Supertypes = superTypes ctx cs |> Set.ofList
          Subtypes = subTypes ctx cs |> Set.ofList }
