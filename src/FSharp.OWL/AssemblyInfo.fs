namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharp.OWL")>]
[<assembly: AssemblyProductAttribute("FSharp.OWL")>]
[<assembly: AssemblyDescriptionAttribute("OWL type provider")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
