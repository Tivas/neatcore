namespace NeatCore

module NetworkVisualizer =
    open NeatCore.Genome
    open System

    let GenomeToGraphWiz(genome: GenomeData) =
        let nameNode (n: NodeGene) =
            let typestring =
                match n.Type with
                | Hidden -> sprintf "H"
                | Input -> sprintf "I"
                | Output -> sprintf "O"
            sprintf "%s%i" typestring n.Id

        let connections =
            genome.Connections
            |> Map.toArray
            |> Array.map snd
            |> Array.filter (fun x -> x.IsEnabled && not (x.FromNode.Type = Input && x.ToNode.Type = Output))
            |> Array.map (fun c ->
                let connstring =
                    sprintf @"%s -> %s [label=""I%i\n(%.2f)""]" (nameNode c.FromNode) (nameNode c.ToNode)
                        c.InnovationNumber c.Weight
                connstring)

        let nodestrings =
            let inputs =
                genome.Nodes
                |> Array.filter (fun gn -> gn.Type = Input)
                |> Array.map (fun i -> sprintf "%s" (nameNode i))
                |> String.concat (sprintf ";%s" Environment.NewLine)
            let hidden =
                genome.Nodes
                |> Array.filter (fun gn -> gn.Type = Hidden)
                |> Array.map (fun i -> sprintf "%s" (nameNode i))
                |> String.concat (sprintf ";%s" Environment.NewLine)
            let outputs =
                genome.Nodes
                |> Array.filter (fun gn -> gn.Type = Output)
                |> Array.map (fun i -> sprintf "%s" (nameNode i))
                |> String.concat (sprintf ";%s" Environment.NewLine)

            let inputsSub = sprintf @"subgraph clusterInputs { style=""invis"" label = ""Inputs"" %s}" inputs
            //let hiddenSub = sprintf @"subgraph clusterHidden { label = ""Hidden"" %s}" hidden
            let hiddenSub = sprintf @" %s" hidden
            let outputsSub = sprintf @"subgraph clusterOutput { style=""invis"" label = ""Outputs"" %s }" outputs
            [ inputsSub; hiddenSub; outputsSub ] |> String.concat Environment.NewLine

        let connstrings = connections |> String.concat (sprintf ";%s" Environment.NewLine)
        sprintf "digraph G { node [shape=circle]  %s %s %s %s }" Environment.NewLine nodestrings connstrings
            Environment.NewLine
