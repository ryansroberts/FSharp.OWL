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

let domainMap (o : OWLOntology) (r : OWLReasoner) (f : OWLDataFactory) = 
    let characteristicsOf (p : OWLObjectProperty) = 
        let test f p = 
            if f then p
            else Characteristics.None

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
    
    let domainMap px root = 
        px
        |> iter<OWLObjectProperty>
        |> List.map (fun p ->
               let c = f.getOWLObjectSomeValuesFrom (p, f.getOWLThing())
               let eq = r.getEquivalentClasses(c).getEntities() |> iter<OWLClass>
               
               let sup = 
                   eq
                   |> List.map (fun c -> ((r.getSubClasses(c, true).getFlattened()) |> iter<OWLClass>))
                   |> List.concat
               eq @ sup |> List.map (fun c -> (c, characteristicsOf p, p)))
        |> Seq.concat
        |> Seq.groupBy (fun (c, _, _) -> c)
        |> Seq.map (fun (c, p) -> (c, p |> Seq.map (fun (c, ch, p) -> (ch, p))))
        |> Map.ofSeq
    
    (domainMap (o.getObjectPropertiesInSignature()) (f.getOWLThing()), 
     (domainMap (o.getDataPropertiesInSignature()) (f.getTopDatatype())))

type ReasoningContext = 
    { Ontology : OWLOntology
      Reasoner : OWLReasoner
      DataFactory : OWLDataFactory
      ObjectDomain : Map<OWLClass, (Characteristics * OWLObjectProperty) seq>
      DataDomain : Map<OWLClass, (Characteristics * OWLObjectProperty) seq> }
    static member create (o, r, f) = 
        match o, r, f with
        | Ontology o, Reasoner r, Factory f -> 
            let (oprops, dataprops) = domainMap o r f
            { Ontology = o
              Reasoner = r
              DataFactory = f
              ObjectDomain = oprops
              DataDomain = dataprops }

let rec splitIntersections (c : obj) = 
    [ match c with
      | :? OWLObjectIntersectionOf as x -> 
          yield! x.getClassesInSignature()
                 |> iter<OWLEntity>
                 |> List.map splitIntersections
                 |> List.concat
      | :? HasIRI as x -> yield Uri.fromHasUri x ]

let extractIri ex = ex |> List.map Uri.fromHasUri

let objectProperties ctx (c : OWLClass) = 

    
    let rangeOf c p = 
        let possible = ctx.DataFactory.getOWLObjectSomeValuesFrom (ctx.DataFactory.getOWLObjectInverseOf (p), c)
        match ctx.Reasoner.getEquivalentClasses(possible).getEntities() |> iter<OWLClass> with
        | [] -> ctx.Reasoner.getSuperClasses(possible, true).getFlattened() |> iter<OWLClass>
        | eq -> eq
    
    let inClosure (cx : OWLClass list) = cx |> List.filter (fun c -> ctx.Ontology.containsEntityInSignature (c, true))
    let cardinality c p = Cardinality.Unspecified
    ctx.ObjectDomain
    |> Map.find c
    |> Seq.map (fun (ch, p) -> 
           (Uri.fromHasUri p, ch, 
            { Range = 
                  rangeOf c p
                  |> inClosure
                  |> extractIri
                  |> Set.ofList
              Cardinality = cardinality c p }))

let subTypes ctx (c : OWLClass) = 
    ctx.Reasoner.getSubClasses(c, true).getFlattened()
    |> iter<OWLEntity>
    |> List.map splitIntersections
    |> List.concat

let superTypes ctx (c : OWLClass) = 
    ctx.Reasoner.getSuperClasses(c, true).getFlattened()
    |> iter<OWLEntity>
    |> List.map splitIntersections
    |> List.concat

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
          Label = [] |> Set.ofList
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
