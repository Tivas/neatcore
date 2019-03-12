namespace neatcore

open Genome
open TensorFlow

module Substrate = 
    type Substrate = { Graph : TFGraph }
    
    let run substrate = 
        let session = new TFSession(substrate)
        ""

module SubstrateCompiler =
    type Compile = GenomeData -> Substrate.Substrate

    let rec Compile (genome:GenomeData) =
        let mapNode (graph:TFGraph) = function
            | Input d -> graph.Placeholder(TFDataType.Float, TFShape.Scalar, string d.InnovationNumber)
            | _ ->  graph.Placeholder(TFDataType.Float, TFShape.Scalar, "unknown")

        let mapConnection (graph:TFGraph) conn = graph

        let graph = new TFGraph()
       // let nodes = genome.Nodes |> List.map (fun n -> graph.Constant(42, TFShape.Scalar) )

        { Substrate.Substrate.Graph = graph }
