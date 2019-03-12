namespace neatcore 

module NetworkVisualizer =
    open neatcore.Genome
    open System

    let GenomeToGraphWiz (genome:GenomeData) = 
        let nameNode = function
        | Hidden i -> sprintf "H%i" i.InnovationNumber
        | Input i -> sprintf "I%i" i.InnovationNumber
        | Output i -> sprintf "O%i" i.InnovationNumber

        let connections = 
            genome.Connections 
            |> List.filter (fun x -> x.IsEnabled)
            |> List.map (fun c -> 

                let connstring = sprintf @"%s -> %s [label=""I%i\n(%.2f)""]" (nameNode c.FromNode) (nameNode c.ToNode) c.InnovationNumber c.Weight
                connstring )
        let nodestrings = 
            let inputs = genome.Nodes |> List.choose (fun n -> match n with | Input _ -> Some n |_ -> None) |> List.map (fun i -> sprintf "%s" (nameNode i)) |> String.concat (sprintf ";%s" Environment.NewLine)
            let hidden = genome.Nodes |> List.choose (fun n -> match n with | Hidden _ -> Some n |_ -> None) |> List.map (fun i -> sprintf "%s" (nameNode i)) |> String.concat (sprintf ";%s" Environment.NewLine)
            let outputs = genome.Nodes |> List.choose (fun n -> match n with | Output _ -> Some n |_ -> None) |> List.map (fun i -> sprintf "%s" (nameNode i)) |> String.concat (sprintf ";%s" Environment.NewLine)
            let inputsSub = sprintf @"subgraph clusterInputs { style=""invis"" label = ""Inputs"" %s}" inputs 
            //let hiddenSub = sprintf @"subgraph clusterHidden { label = ""Hidden"" %s}" hidden 
            let hiddenSub = sprintf @" %s" hidden 
            let outputsSub = sprintf @"subgraph clusterOutput { style=""invis"" label = ""Outputs"" %s }" outputs
            [inputsSub ; hiddenSub ; outputsSub] |> String.concat Environment.NewLine 
        let connstrings = connections  |> String.concat (sprintf ";%s" Environment.NewLine)
        sprintf "digraph G { node [shape=circle]  %s %s %s %s }" Environment.NewLine nodestrings connstrings Environment.NewLine
