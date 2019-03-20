// Learn more about F# at http://fsharp.org

open System
open neatcore.Population
open neatcore.Phenotype
open neatcore
open System.IO

[<EntryPoint>]
let main argv =
    let random = new Random(int DateTime.Now.Ticks)
    let pop = Init random 1 2 1 
    let mutable newPop = MutatePopulation random pop
    for e in [1..10] do
        newPop <- MutatePopulation random newPop 
    //let graphs = newPop.Generations |> List.head |> List.head |> SubstrateCompiler.Compile
    let stringss = newPop.Generations.[0].[0] |> NetworkVisualizer.GenomeToGraphWiz
    File.WriteAllText("generation2.gw", stringss) |> ignore
//    Console.Read() |> ignore
    0 // return an integer exit code
