namespace global
module Store =   
    open VDS.RDF
    open VDS.RDF.Query
    open VDS.RDF.Query.Datasets
    open VDS.RDF.Storage
    open VDS.RDF.Storage.Management
    open VDS.RDF.Parsing
    open Schema
    
    let parser = SparqlQueryParser()
    
    type store = 
        | Memory of IGraph
        member x.QueryProcessor() = 
            match x with
            | Memory m -> LeviathanQueryProcessor(InMemoryDataset(m))
    
    let defaultUri = null :> System.Uri
    let loadGraph (g : Graph) = store.Memory g
    
    let loadFile (s : string) = 
        let g = new Graph()
        match s.StartsWith("http") with
        | true -> g.LoadFromUri(System.Uri s)
        | false -> g.LoadFromFile s
        Memory g
    
    let addNodes (t : SparqlParameterizedString) px = 
        for (n, v) in px do
            match v with
            | Node.Uri(Schema.Uri(uri)) -> t.SetUri(n, System.Uri uri)
            | Node.Literal(Schema.Literal(s)) -> t.SetLiteral(n, s)
            | Node.Uri(Blank(s)) -> t.SetBlankNode(s)
    

   

    let translate (q : Sparql)= 
        let pattern = 
            function 
            | QueryPattern.Binding(Schema.Binding(b)) -> "?" + b
            | QueryPattern.OfType -> "a"
            | QueryPattern.Node n -> 
                match n with
                | Node.Uri(Schema.Uri(u)) -> sprintf "<%s>" u
                | Literal(Schema.Literal(l)) -> sprintf "\"%s\"" l
        
        let select bx = 
            sprintf "select %s " (bx |> (List.fold (fun a b -> 
                                                match b with
                                                | Binding.Binding b -> a + "?" + b + " "
                                                | Binding.Wildcard -> a + "*" + " ") ""))
        
        let queryType = function 
            | Select bx -> select bx
        let where = 
            function 
            | Where wx -> 
                wx 
                |> List.fold (fun a (BGP(s, p, o)) -> a + sprintf "%s %s %s . \n" (pattern s) (pattern p) (pattern o)) 
                       ""
        let (qt, w) = q
        sprintf "%s \n where { %s }" (queryType qt) (where w)

    let query q (store:store) =
        printfn "%s" (translate q) 
        (store.QueryProcessor()).ProcessQuery(parser.ParseFromString(translate q ))
    
    let construct q (store : store) = query q store :?> IGraph
    let resultset q (store : store) = query q store :?> SparqlResultSet
    
    let dump g = 
        let s = System.Text.StringBuilder()
        let w = new VDS.RDF.Writing.CompressingTurtleWriter()
        use sw = new System.IO.StringWriter(s)
        w.Save(g, sw)
        s.ToString()

module Prefixes = 
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    let rdfs = "http://www.w3.org/2000/01/rdf-schema#"
    let owl = "http://www.w3.org/2002/07/owl#"

module Queries = 
    open Store
