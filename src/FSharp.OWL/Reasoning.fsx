#if INTERACTIVE
#load "GenerateSetupScript.fsx"

GenerateSetupScript.generateSetupScript __SOURCE_DIRECTORY__ "FsRdf"
#load "__setup__FsRdf__.fsx"

#else
module ReasoningTest
#endif

open Reasoning

let (++) a b = System.IO.Path.Combine (a,b)
let r = Reasoner()
let o = r.loadFile (__SOURCE_DIRECTORY__ ++ "wine.ttl")

r.schema o "http://www.w3.org/TR/2003/PR-owl-guide-20031209/food#Wine"

