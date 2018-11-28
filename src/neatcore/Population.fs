namespace neatcore

module Population =
    open Genome

    type Population =
        {
            ConnectionIncrementer: unit -> float
            NodeIncrementer: unit -> float
            Generations : Genome list
        }


    let MutatePopulation random population = population

    let EvaluateGenomes genomes = []

    let Speciate genomes = [] // species list

    let CrossoverGenomes = []

    let Init inputs outputs =
        let incrementer = Util.incrementer
        let nodeIncrementer = Util.incrementer

        let randomGenerator = Util.randomizer -2.0 2.0
        let randomGeneratorZeroToTwo = Util.randomizer 0.0 2.0

        let inputs = [1..inputs] |> List.map (fun x -> NodeGene.Input {InnovationNumber = nodeIncrementer() }) 
        let outputs = [1..outputs] |> List.map (fun x -> NodeGene.Output {InnovationNumber = nodeIncrementer() }) 
        let genomes =
            { 
                Nodes = inputs @ outputs
                Connections = []
            } 
        ""

        //{ ConnectionIncrementer = incrementer}
        