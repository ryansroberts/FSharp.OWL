module TypeProviderInstantiation

open System
open System.IO
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Providers

type FileProviderArgs =  { Path : string
                           BaseUri : string
                           NSMap : string }

type TypeProviderInstantiation = 
    | FileProvider of FileProviderArgs
    
    member x.GenerateType resolutionFolder runtimeAssembly = 
        let f, args = 
            match x with
            | FileProvider x -> 
                (fun cfg -> new Memory(cfg) :> TypeProviderForNamespaces), 
                [| box x.Path
                   box x.BaseUri
                   box x.NSMap |]
        Debug.generate resolutionFolder runtimeAssembly f args
    
    override x.ToString() = 
        match x with
        | FileProvider x -> [ "FileProvider"; x.Path; x.NSMap ]
        |> String.concat ","
    
    member x.ExpectedPath outputFolder = 
        Path.Combine
            (outputFolder,"wine.txt")
    member x.Dump resolutionFolder outputFolder runtimeAssembly signatureOnly ignoreOutput = 
        let replace (oldValue : string) (newValue : string) (str : string) = str.Replace(oldValue, newValue)
        
        let output = 
            x.GenerateType resolutionFolder runtimeAssembly
            |> match x with
               | _ -> Debug.prettyPrint false false 10 100
            |> replace "FSharp.Data.Runtime." "FDR."
            |> if String.IsNullOrEmpty resolutionFolder then id
               else replace resolutionFolder "<RESOLUTION_FOLDER>"
        if outputFolder <> "" then File.WriteAllText(x.ExpectedPath outputFolder, output)
        output

open System.Runtime.CompilerServices
