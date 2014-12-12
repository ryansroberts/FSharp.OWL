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
    o.getObjectPropertiesInSignature()
    |> iter<OWLObjectProperty>
    |> List.map (fun p -> 
           let c = f.getOWLObjectSomeValuesFrom (p, f.getOWLThing())
           match r.getEquivalentClasses(c).getEntities() |> iter<OWLClass> with
           | [] -> 
               [ for c in r.getSuperClasses(c, true).getFlattened() |> iter<OWLClass> -> (c, p) ]
           | ex -> 
               [ for c in ex -> (c, p) ])
    |> Seq.concat
    |> Seq.groupBy (fun (c, p) -> c)
    |> Seq.map (fun (c, p) -> (c, Seq.map (snd) p))
    |> Map.ofSeq

type ReasoningContext = 
    { Ontology : OWLOntology
      Reasoner : OWLReasoner
      DataFactory : OWLDataFactory
      ObjectDomain : Map<OWLClass, OWLObjectProperty seq> }
    static member create (o, r, f) = 
        match o, r, f with
        | Ontology o, Reasoner r, Factory f -> 
            { Ontology = o
              Reasoner = r
              DataFactory = f
              ObjectDomain = domainMap o r f }

let rec splitIntersections (c : obj) = 
    [ match c with
      | :? OWLObjectIntersectionOf as x -> 
          yield! x.getClassesInSignature()
                 |> iter<OWLEntity>
                 |> List.map splitIntersections
                 |> List.concat
      | :? HasIRI as x -> yield Uri.fromHasUri x ]

let extractIri (nx : java.util.Set) = iter<HasIRI> nx |> List.map Uri.fromHasUri

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
            let reasonerFactory = HermiT.Net.NetReasonerFactoryImpl() 
            let config = new SimpleConfiguration()
            
            let reasoner = reasonerFactory.createReasoner (o, config)
            Ontology(reasoner.getRootOntology()), Reasoner reasoner, 
            Factory(o.getOWLOntologyManager().getOWLDataFactory())
    
    member x.schema ctx (iri : string) = 
        let cs = (ctx.DataFactory.getOWLClass(IRI.create iri).asOWLClass())
        { Uri = Uri.Uri(iri)
          Label = [] |> Set.ofList
          ObjectProperties = Set.empty
          DataProperties = Set.empty
          EquivalentClasses = 
              ctx.Reasoner.getEquivalentClasses(cs).getEntities()
              |> iter<obj>
              |> List.map splitIntersections
              |> List.concat
              |> Set.ofList
          Supertypes = superTypes ctx cs |> Set.ofList
          Subtypes = subTypes ctx cs |> Set.ofList }
