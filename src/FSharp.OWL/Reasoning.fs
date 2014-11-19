﻿module Reasoning

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


type Ontology = 
    | Ontology of OWLOntology

type Uri with
    static member fromIRI (iri : IRI) = Uri.Uri(iri.toString())
    static member fromHasUri (has : HasIRI) = Uri.Uri((has.getIRI()).toString())

let iter<'a> (nx : java.util.Set) = 
        [ let i = nx.iterator()
          while i.hasNext() do
              yield i.next() :?> 'a ]


let rec splitIntersections (c:obj) = [
    match c with 
    | :? OWLObjectIntersectionOf as x ->
        yield! x.getClassesInSignature() 
                |> iter<OWLEntity>
                |> List.map splitIntersections 
                |> List.concat
    | :? HasIRI as x -> yield Uri.fromHasUri x
]

let extractIri (nx : java.util.Set) = 
    iter<HasIRI> nx
    |> List.map Uri.fromHasUri

let extractDataProperties (o : OWLOntology) (nx : java.util.Set) = 
    iter<OWLObjectProperty> nx
    |> List.map (fun p -> (Uri.Uri((p.getIRI()).ToString()), p.getRanges (o) |> extractIri))
    |> Set.ofList

let extractObjectProperties (o : OWLOntology) (nx : java.util.Set) = 
    iter<OWLObjectProperty> nx
    |> List.map (fun p -> (Uri.Uri((p.getIRI()).ToString()), p.getRanges (o) |> extractIri))
    |> Set.ofList

let propertiesFrom (o : OWLOntology) (c : OWLClass) = 
    c.getSuperClasses (o)
    |> iter<OWLClassExpression>
    |> List.map (fun l -> 
           l.getObjectPropertiesInSignature()
           |> iter<OWLObjectProperty>
           |> List.map (fun p -> 
                  (Uri.fromHasUri p, 
                   p.getRanges (o)
                   |> iter<HasIRI>
                   |> List.map Uri.fromHasUri
                   |> Set.ofList)))
    |> List.concat
    |> Set.ofList

let dataPropertiesFrom (o : OWLOntology) (c : OWLClass) = 
    c.getSuperClasses (o)
    |> iter<OWLClassExpression>
    |> List.map (fun l -> 
           l.getDataPropertiesInSignature()
           |> iter<OWLProperty>
           |> List.map (fun p -> 
                  (Uri.fromHasUri p, 
                   p.getRanges (o)
                   |> iter<HasIRI>
                   |> List.map Uri.fromHasUri
                   |> Set.ofList)))
    |> List.concat
    |> Set.ofList 


let subTypes (o:OWLOntology) (c:OWLClass) =
    c.getSubClasses(o)
    |> iter<OWLEntity>
    |> List.map splitIntersections
    |> List.concat

type Reasoner () =
    member x.manager = OWLManager.createOWLOntologyManager()
    member x.loadFile (p : string) = 
        try 
            Ontology(x.manager.loadOntologyFromOntologyDocument (java.io.File(p)))
        with :? OWLOntologyAlreadyExistsException as e -> 
            Ontology(x.manager.getOntology (e.getDocumentIRI())) |> x.reason
    member private x.reason o = 
        match o with
        | Ontology(o) -> 
            let reasonerFactory = StructuralReasonerFactory()
            let config = new SimpleConfiguration()
            let reasoner = reasonerFactory.createReasoner (o, config)
            reasoner.precomputeInferences()
            Ontology o
    member x.schema o (iri : string) = 
        match o with
        | Ontology.Ontology(o) -> 
            let f = x.manager.getOWLDataFactory()
            let cs = (f.getOWLClass(IRI.create iri).asOWLClass())

            { Uri = Uri.Uri(iri)
              Label = [] |> Set.ofList
              ObjectProperties =  propertiesFrom o cs
              DataProperties = Set.empty
              EquivalentClasses =  cs.getEquivalentClasses(o) 
                                    |> iter<obj>
                                    |> List.map splitIntersections 
                                    |> List.concat
                                    |> Set.ofList
              Supertypes = Set.empty
              Subtypes = subTypes o cs |> Set.ofList
            }
