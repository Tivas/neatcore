namespace NeatCore

module Genome =
    open System
    open System.Data

    type NodeGeneType =
        | Input
        | Hidden
        | Output

    type NodeGene =
        { Id: int
          Type: NodeGeneType }

    type ConnectionGene =
        { FromNode: NodeGene
          ToNode: NodeGene
          Weight: float
          IsEnabled: bool
          InnovationNumber: int }

    type GenerationConnection =
        { FromNode: NodeGene
          ToNode: NodeGene
          InnovationNumber: int }

    type GenomeData =
        { Nodes: NodeGene array
          Connections: Map<NodeGene * NodeGene, ConnectionGene> }

    type Genome =
        | EvaluatedGenome of GenomeData * fitness: float
        | NonEvaluatedGenome of GenomeData

    let AddConnection (genome : GenomeData) (con: ConnectionGene) : GenomeData = 
        let cons = genome.Connections
        let lookup = (con.FromNode, con.ToNode)
        let newGenomeConnections = 
            match cons.ContainsKey lookup with
            | true -> 
                let newCons = cons.Remove lookup
                (lookup, con) |> newCons.Add 
            | false -> 
                (lookup, con) |> cons.Add 
        { genome with Connections = newGenomeConnections }

    let generateInnovationNumber (generationConnections: GenerationConnection array) fromNode toNode incr =
        let foundConnection =
            generationConnections |> Array.tryFind (fun x -> x.FromNode = fromNode && x.ToNode = toNode)
        match foundConnection with
        | Some c ->
            printfn "Took already found innovationnumber"
            c.InnovationNumber
        | None -> incr()

    // Mutation
    let MutateAddConnection random incr generationConnections genome =
        let fromNodes =
            genome.Nodes
            |> Array.filter (fun n -> n.Type = Input || n.Type = Hidden)
            |> Util.shuffle


        let findToNode fromNodes =
            let potentialToNodes = genome.Nodes |> Array.filter (fun n -> n.Type = Output || n.Type = Hidden)

            fromNodes
            |> Array.tryPick (fun fn ->
                potentialToNodes
                |> Array.tryPick (fun ptn ->
                    // let conn = genome.Connections |> Mapts (fun c -> c.FromNode = fn && c.ToNode = ptn)
                    let conn = genome.Connections |> Map.containsKey (fn,ptn)
                    if not conn then Some(fn, ptn) else None))

        findToNode fromNodes
        |> Option.map (fun (fromNode, toNode) ->
            let innovationNumber = generateInnovationNumber generationConnections fromNode toNode incr

            let randomWeight = random()

            let link =
                { FromNode = fromNode
                  ToNode = toNode
                  IsEnabled = true
                  Weight = randomWeight
                  InnovationNumber = innovationNumber }

            let generationConnection =
                { FromNode = fromNode
                  ToNode = toNode
                  InnovationNumber = innovationNumber }

            let newGenome = AddConnection genome link

            (newGenome, Some([| generationConnection |])))
        |> Option.defaultValue (genome, None)

    let MutateAddNode random randomWeight connIncr nodeIncr (generationConnections: GenerationConnection array) (genome : GenomeData) = 
        let newNode =
            { Id = nodeIncr()
              Type = Hidden }
        
        let randomConnection =
            genome.Connections
            |> Map.toArray
            |> Array.map snd
            |> Array.filter (fun x -> x.IsEnabled)
            |> Util.randomElement random

        let incomingConnection =
            { FromNode = randomConnection.FromNode
              ToNode = newNode
              IsEnabled = true
              InnovationNumber =
                  generateInnovationNumber generationConnections randomConnection.FromNode newNode connIncr
              Weight = 1.0 }

        let outgoingConnection =
            { FromNode = newNode
              ToNode = randomConnection.ToNode
              IsEnabled = true
              InnovationNumber =
                  generateInnovationNumber generationConnections newNode randomConnection.ToNode connIncr
              Weight = randomWeight() }

        let disabledConnection = { randomConnection with IsEnabled = false }

        let newConnections = 
            [| disabledConnection; incomingConnection; outgoingConnection |]


        let generationConnections =
            newConnections
            |> Array.map (fun c ->
                { FromNode = c.FromNode
                  ToNode = c.ToNode
                  InnovationNumber = c.InnovationNumber })

        // Add node to any random connection
        // disable connection add two new
        // incoming connection with weight 1
        // outgoing weight to the same as the old connection
        let newGenome =
            { genome with
                  Nodes = Array.append [| newNode |] genome.Nodes }

        let newGenomeWithConnections =
            newConnections
            |> Array.fold (fun a c -> (AddConnection a c) ) newGenome

        newGenomeWithConnections, Some(generationConnections)

    let MutateEnableDisableConnection random genome =
        let randomConnection =
            genome.Connections
            |> Map.toArray
            |> Array.map snd
            |> Array.filter (fun x -> x.IsEnabled)
            |> Util.randomElement random

        // find a random connection and switch IsEnabled
        let newConnection = { randomConnection with IsEnabled = not randomConnection.IsEnabled }
        AddConnection genome newConnection

    let MutateWeightShift random randomWeight genome = // RANDOM 0 to 2
        let randomConnection =
            genome.Connections
            |> Map.toArray
            |> Array.map snd
            |> Array.filter (fun x -> x.IsEnabled)
            |> Util.randomElement random

        let randomNumber = randomWeight()
        // find a random connection and switch IsEnabled
        let newConnection = { randomConnection with Weight = randomConnection.Weight * randomNumber }
        AddConnection genome newConnection
    // random weight multiplied by a number between 0 or 2

    let MutateWeightRandom random randomWeight genome =
        let randomConnection =
            genome.Connections
            |> Map.toArray
            |> Array.map snd
            |> Array.filter (fun x -> x.IsEnabled)
            |> Util.randomElement random

        let randomNumber = randomWeight()
        let newConnection = { randomConnection with Weight = randomNumber }
        AddConnection genome newConnection
    // random weight set to random number between -2 and 2