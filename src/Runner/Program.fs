// Learn more about F# at http://fsharp.org

open System
open NeatCore.Population
open NeatCore
open System.IO

[<EntryPoint>]
let main argv =
    let random = new Random() // Same seed for debugging
    let population = PopulationAgent random 10 10 10 
    population.Post(EvolveGeneration)
    let stringss = 
        population.PostAndReply (GetMostFit) |> NetworkVisualizer.GenomeToGraphWiz
    File.WriteAllText("generation2.gw", stringss) |> ignore
//    Console.Read() |> ignore
    0 // return an integer exit code
