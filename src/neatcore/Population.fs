namespace NeatCore

module Population =
    open Genome

    type Generation =
        { Genomes: GenomeData list
          AddedConnections: Genome.GenerationConnection list }

    type PopulationCmd =
        | EvolveGeneration
        | GetMostFit of AsyncReplyChannel<GenomeData>

    type PopulationAgent = (unit -> float) -> int -> int -> int -> MailboxProcessor<PopulationCmd>

    // let MutatePopulation random population =
    //     let newGeneration =
    //         population.Generations
    //         |> List.head
    //         |> List.map (fun g -> (Util.shuffle population.Mutators |> Seq.head) g)
    //     { population with Generations = newGeneration :: population.Generations }

    let EvaluateGenomes genomes = []

    let Speciate genomes = [] // species list

    let CrossoverGenomes = []

    let LiftGenomeFunc(f: GenomeData -> GenomeData): GenerationConnection array -> GenomeData -> (GenomeData * GenerationConnection array option)
        = fun _ g -> (g, None)


    let InitMutators
        random
        connectionIncrementer
        nodeIncrementer
        : (GenerationConnection array -> GenomeData -> (GenomeData * GenerationConnection array option)) array
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
            let cons = initialConnections |> Array.map (fun c -> { c with Weight = randomGeneratorMinusTwoToTwo() }) 
            let genome = 
                { Nodes = (inputs |> Array.append outputs)
                  Connections = Map.empty }
            yield (cons |> Array.fold (fun a c -> (Genome.AddConnection a c) ) genome)
        ]

    let PopulationAgent random (size: int) (numOfInputs: int) (numOfOutputs: int) =
        let connectionIncrementer = Util.incrementer()
        let nodeIncrementer = Util.incrementer()
        let initGeneration = Init random connectionIncrementer nodeIncrementer size numOfInputs numOfOutputs
        let mutators = InitMutators random connectionIncrementer nodeIncrementer
        MailboxProcessor.Start(fun inbox ->
            let rec loop (genomes: GenomeData list) fittedGenomes =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | EvolveGeneration ->
                        let doGetMutator() =
                            Util.randomElement random mutators

                        let mutatedGeneration =
                            genomes
                            |> List.fold (fun (mgs, mutatedLinks) g ->
                                let (mutatedGenome, link) = doGetMutator() mutatedLinks g
                                let genomes = mutatedGenome :: mgs

                                let links =
                                    match link with
                                    | Some(l) -> l |> Array.append mutatedLinks
                                    | None -> mutatedLinks
                                (genomes, links)) ([], [||])

                        return! loop (fst mutatedGeneration) []
                    | GetMostFit rc ->
                        rc.Reply(genomes |> List.head)
                        return! loop genomes fittedGenomes
                    | _ -> return! loop genomes fittedGenomes
                }
            loop initGeneration [])
