// Learn more about F# at http://fsharp.org

open System
open NeatCore.Population
open NeatCore.Genome
open NeatCore
open System.IO

[<EntryPoint>]
let main argv =
    let random = Random() // Same seed for debugging
    let population = PopulationAgent random 250 700 10 
    for _ in 0 .. 10000 do
        let agent = population.PostAndReply (GetNextGenome)
        population.Post(EvolveGenome (agent.Id, random.NextDouble()))

    let theDude = population.PostAndReply (GetMostFit) 
    let stringss = NetworkVisualizer.GenomeToGraphWiz theDude.Data 
    File.WriteAllText("generation2.gw", stringss) |> ignore
//    Console.Read() |> ignore
    0 // return an integer exit code
