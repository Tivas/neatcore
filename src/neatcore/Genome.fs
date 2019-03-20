namespace neatcore

module Genome =
    open System

    type NodeData = 
        {
            InnovationNumber : int
        }

    type NodeGene = 
    | Input of NodeData
    | Hidden of NodeData
    | Output of NodeData

    type ConnectionGene = 
        { 
            FromNode : NodeGene 
            ToNode : NodeGene 
            Weight : float
            IsEnabled : bool
            InnovationNumber : int
        } 

    type GenomeData =  
        {
            Nodes : NodeGene list 
            Connections : ConnectionGene list
        } 

    type Genome =
    | EvaluatedGenome of GenomeData * fitness:float
    | NonEvaluatedGenome of GenomeData

    // Mutation
    let MutateAddConnection random incr allConnections genome = 
        let fromNode = 
            genome.Nodes 
            |> Seq.choose 
                (fun x ->
                    match x with
                    | Input _ -> Some(x)
                    | Hidden _ -> Some(x)
                    | _ -> None)
            |> Util.shuffle 
            |> Seq.head

        let toNode = 
            genome.Nodes 
            |> Seq.choose 
                (fun x ->
                    match x with
                    | Output _ -> Some(x)
                    | Hidden _ -> Some(x)
                    | _ -> None)
            |> Util.shuffle 
            |> Seq.head

        let innovationNumber = 
            let foundConnection = allConnections |> Seq.tryFind (fun x -> fromNode = fromNode && toNode = toNode)
            match foundConnection with
            | Some c -> c.InnovationNumber
            | None -> incr()

        let randomWeight = random()

        let link = 
            {
                FromNode = fromNode
                ToNode = toNode
                IsEnabled = true
                Weight = randomWeight
                InnovationNumber = innovationNumber
            }
        // Add link between two nodes
        // Weight is random -2 to 2 
        // if connection exist already. use the innovationNumber
        { genome with 
            Connections = link :: genome.Connections
        }
        
    let MutateAddNode random connIncr nodeIncr genome =  // TODO: InnovationNumber generator
        let newNode = Hidden { InnovationNumber = nodeIncr() }
        let randomConnection = genome.Connections |> List.filter (fun x -> x.IsEnabled) |> Util.shuffle |> Seq.head 
        let incomingConnection = 
            { 
                FromNode = randomConnection.FromNode
                ToNode = newNode
                IsEnabled = true
                InnovationNumber = connIncr()
                Weight = 1.0
            }

        let outgoingConnection =
            { 
                FromNode = newNode
                ToNode = randomConnection.ToNode
                IsEnabled = true
                InnovationNumber = connIncr()
                Weight = random()
            }
        
        let disabledConnection = { randomConnection with IsEnabled = false }

        let newConnections = disabledConnection :: incomingConnection :: outgoingConnection :: (genome.Connections |> List.except [randomConnection])

        // Add node to any random connection
        // disable connection add two new 
        // incoming connection with weight 1
        // outgoing weight to the same as the old connection
        { genome with 
            Connections = newConnections
            Nodes = newNode :: genome.Nodes
        }
        
    let MutateEnableDisableConnection genome = 
        let randomConnection = genome.Connections |> Util.shuffle |> Seq.head 
        // find a random connection and switch IsEnabled
        let newConnection = { randomConnection with IsEnabled = not randomConnection.IsEnabled }
        let newConnections = newConnection :: (genome.Connections |> List.except [randomConnection] )
        { genome with Connections = newConnections } 

    let MutateWeightShift random genome = // RANDOM 0 to 2
        let randomConnection = genome.Connections |> Util.shuffle |> Seq.head 
        let randomNumber = random() 
        // find a random connection and switch IsEnabled
        let newConnection = { randomConnection with Weight = randomConnection.Weight * randomNumber }
        let newConnections = newConnection :: (genome.Connections |> List.except [randomConnection] )
        { genome with Connections = newConnections } 
        // random weight multiplied by a number between 0 or 2
        
    let MutateWeightRandom random genome = 
        let randomConnection = genome.Connections |> Util.shuffle |> Seq.head 
        let randomNumber = random()
        // find a random connection and switch IsEnabled
        let newConnection = { randomConnection with Weight = randomNumber }
        let newConnections = newConnection :: (genome.Connections |> List.except [randomConnection] )
        { genome with Connections = newConnections } 
        // random weight set to random number between -2 and 2

    // Crossover
    let Cross moreFit lessFit =
        ""

    // Selection
    let MatchToSpecies = "" 
