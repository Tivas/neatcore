namespace NeatCore

open System
open FSharpx.Collections

module Population =
    open Genome

    type Generation =
        { Genomes: GenomeData list
          AddedConnections: Genome.GenerationConnection list }

    type Fitness = float

    type PopulationCmd =
        | GetMostFit of AsyncReplyChannel<EvaluatedGenome>
        | GetNextGenome of AsyncReplyChannel<NonEvaluatedGenome>
        | EvolveGenome of Guid * Fitness

    type PopulationAgent = System.Random -> int -> int -> int -> MailboxProcessor<PopulationCmd>

    let LiftGenomeFunc(f: GenomeData -> GenomeData): GenerationConnection list -> GenomeData -> (GenomeData * GenerationConnection list option)
        = fun _ g -> (g, None)


    let InitMutators
        random
        connectionIncrementer
        nodeIncrementer
        : (GenerationConnection list -> GenomeData -> (GenomeData * GenerationConnection list option)) array
        =
        let randomGeneratorMinusTwoToTwo = Util.randomizer random -2.0 2.0
        let randomGeneratorZeroToTwo = Util.randomizer random 0.0 2.0

        let addConnection = Genome.MutateAddConnection randomGeneratorMinusTwoToTwo connectionIncrementer
        let addNode = Genome.MutateAddNode random randomGeneratorMinusTwoToTwo connectionIncrementer nodeIncrementer
        let enableDisableConnection = Genome.MutateEnableDisableConnection random |> LiftGenomeFunc
        let weightRandom = Genome.MutateWeightRandom random randomGeneratorZeroToTwo |> LiftGenomeFunc
        let weightShift = Genome.MutateWeightShift random randomGeneratorMinusTwoToTwo |> LiftGenomeFunc

        [| addConnection; addNode; enableDisableConnection; weightRandom; weightShift |]

    let Init random connectionIncrementer nodeIncrementer size numOfinputs numOfoutputs =
        let randomGeneratorMinusTwoToTwo = Util.randomizer random -2.0 2.0
        let randomGeneratorZeroToTwo = Util.randomizer random 0.0 2.0


        let inputs =
            [| 1 .. numOfinputs |]
            |> Array.map (fun x ->
                { Id = nodeIncrementer()
                  Type = Input })

        let outputs =
            [| 1 .. numOfoutputs |]
            |> Array.map (fun x ->
                { Id = nodeIncrementer()
                  Type = Output })

        let initialConnections =
            [| for i in inputs do
                for o in outputs do
                    yield { InnovationNumber = connectionIncrementer()
                            Weight = randomGeneratorMinusTwoToTwo()
                            IsEnabled = true
                            FromNode = i
                            ToNode = o } |]

        [ for _ in [ 1 .. size ] do
            let cons = initialConnections
            let emptyGenome = Genome.GenomeData.Zero
            let genome = { emptyGenome with Nodes = (inputs |> Array.append outputs) }
            yield (cons |> Array.fold (fun a c -> (Genome.AddConnection a c)) genome) ]

    let mutateGeneration doGetMutator (genomes: GenomeData list) =
        let mutatedGeneration =
            genomes
            |> List.fold (fun (mgs, mutatedLinks) g ->
                let (mutatedGenome, link) = doGetMutator () mutatedLinks g
                let genomes = mutatedGenome :: mgs

                let links =
                    match link with
                    | Some(l) -> l @ mutatedLinks
                    | None -> mutatedLinks
                (genomes, links)) ([], [])
        (mutatedGeneration |> fst)

    let PopulationAgent random (size: int) (numOfInputs: int) (numOfOutputs: int) =
        let connectionIncrementer = Util.incrementer()
        let nodeIncrementer = Util.incrementer()
        let initGeneration = (Init random connectionIncrementer nodeIncrementer size numOfInputs numOfOutputs)
        let mutators = InitMutators random connectionIncrementer nodeIncrementer
        let doGetMutator() = Util.randomElement random mutators
        MailboxProcessor.Start(fun inbox ->
            let rec loop
                    (readyGenomes: NonEvaluatedGenome list)
                    (currentlyEvaluatingGenomes: Map<GenomeId, NonEvaluatedGenome>)
                    (fittedGenomes: EvaluatedGenome list)
                    (mostFit: EvaluatedGenome)
                =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | EvolveGenome(id, fitness) ->
                        // Move Genome to fittet Genomes
                        let genome = currentlyEvaluatingGenomes.[id]
                        let newCurrently = currentlyEvaluatingGenomes.Remove(id)
                        let newFitted = { Data = genome; Fitness = fitness}
                        let newFittedGenomes = (newFitted) :: fittedGenomes
                        let newMostFit = if mostFit > newFitted then mostFit else newFitted
                        // Check if all genomes have been fitted
                        match readyGenomes, newCurrently.Count with
                        | [], 0 ->
                            printfn "Evolving generation"
                            // Cross
                            // Mutate
                            let newReadyGenomes = mutateGeneration doGetMutator (newFittedGenomes |> List.map (fun x -> x.Data)) //todo:Cross

                            printfn "Done evolving generation"
                            return! loop newReadyGenomes Map.empty [] newMostFit
                        | _ -> return! loop readyGenomes newCurrently newFittedGenomes newMostFit
                    | GetMostFit rc ->
                        rc.Reply(mostFit)
                        return! loop readyGenomes currentlyEvaluatingGenomes fittedGenomes mostFit
                    | GetNextGenome rc ->
                        match readyGenomes with
                        | [] -> return! loop readyGenomes currentlyEvaluatingGenomes fittedGenomes mostFit
                        | rg :: rgs ->
                            let id = rg.Id
                            rc.Reply(rg)
                            let newCurrently = currentlyEvaluatingGenomes.Add(id, rg)
                            return! loop rgs newCurrently fittedGenomes mostFit
                    | _ -> return! loop readyGenomes currentlyEvaluatingGenomes fittedGenomes mostFit
                }
            loop initGeneration Map.empty []
                (initGeneration
                 |> List.head
                 |> (fun g -> { EvaluatedGenome.Data = g ; Fitness = 0.0}))) //Hack to init mostFit
