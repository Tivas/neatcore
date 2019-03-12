namespace neatcore

module Population =
    open Genome
    open System.Xml

    type Population =
        {
            ConnectionIncrementer: unit -> int 
            NodeIncrementer: unit -> int 
            Generations : GenomeData list list
            Mutators : (GenomeData -> GenomeData) list
        }

    let MutatePopulation random population = 
        let newGeneration = population.Generations |> List.head |> List.map (fun g -> (Util.shuffle population.Mutators |> Seq.head) g )
        { population with Generations = newGeneration :: population.Generations }

    let EvaluateGenomes genomes = []

    let Speciate genomes = [] // species list

    let CrossoverGenomes = []

    let Init random size numOfinputs numOfoutputs =
        let connectionIncrementer = Util.incrementer()
        let nodeIncrementer = Util.incrementer()

        let randomGeneratorMinusTwoToTwo = Util.randomizer random -2.0 2.0
        let randomGeneratorZeroToTwo = Util.randomizer random 0.0 2.0

        let AddConnection = MutateAddConnection randomGeneratorMinusTwoToTwo connectionIncrementer []
        let AddNode = MutateAddNode randomGeneratorMinusTwoToTwo connectionIncrementer nodeIncrementer
        let EnableDisableConnection = MutateEnableDisableConnection
        let WeightRandom = MutateWeightRandom randomGeneratorMinusTwoToTwo
        let WeightShift = MutateWeightShift randomGeneratorMinusTwoToTwo

        let mutators = [ AddConnection; AddNode; EnableDisableConnection; WeightRandom; WeightShift]
  //      let mutators = [ AddConnection; AddNode ]

        let inputs = [1..numOfinputs] |> List.map (fun x -> NodeGene.Input {InnovationNumber = nodeIncrementer() }) 
        let outputs = [1..numOfoutputs] |> List.map (fun x -> NodeGene.Output {InnovationNumber = nodeIncrementer() }) 
        let initialConnections = [
            for i in inputs do 
                for o in outputs do
                    yield { 
                        InnovationNumber = connectionIncrementer()
                        Weight = randomGeneratorMinusTwoToTwo()
                        IsEnabled = true
                        FromNode = i
                        ToNode = o
                    }
        ]

        let intialGeneration = 
            [
                for _ in [1..size] do
                    yield { 
                        Nodes = inputs @ outputs
                        Connections = initialConnections |> List.map (fun c -> {c with Weight = randomGeneratorMinusTwoToTwo() }) 
                    } 
            ]
        {
            ConnectionIncrementer = connectionIncrementer
            NodeIncrementer = nodeIncrementer
            Generations = intialGeneration :: []
            Mutators = mutators
        }

        //{ ConnectionIncrementer = incrementer}
        