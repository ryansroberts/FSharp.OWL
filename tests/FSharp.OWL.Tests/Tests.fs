module FSharp.OWL.Tests

open NUnit.Framework
open System
open System.IO
open System.Net
open Providers
open ProviderImplementation.ProvidedTypes
open TypeProviderInstantiation


type Platform = Net40
type fixture () = 
    [<Test>]
    member public x.``Expression tree for sample RDF`` () =
        let (++) a b = Path.Combine(a, b)
        let resolutionFolder = ""
        let outputFolder =  __SOURCE_DIRECTORY__ + "/expected"
        printf "write expressions to %s\r\n" outputFolder
        let assemblyName = "FsRdf.dll"


        let dump signatureOnly ignoreOutput platform saveToFileSystem (inst:TypeProviderInstantiation) =
            let runtimeAssembly = 
                match platform with
                  | Net40 -> ".." ++ "bin" ++ assemblyName
            inst.Dump resolutionFolder (if saveToFileSystem then outputFolder else "") runtimeAssembly signatureOnly ignoreOutput
            |> Console.WriteLine

        let dumpAll inst = 
            dump false false Net40 false inst

        let args = { 
          Path = __SOURCE_DIRECTORY__ ++ "wine.ttl"
          BaseUri = "http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#Wine"
          NSMap = """wine:http://www.w3.org/TR/2003/PR-owl-guide-20031209/wine#"""
        }

        dumpAll (TypeProviderInstantiation.FileProvider (args))

