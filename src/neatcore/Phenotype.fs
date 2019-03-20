namespace neatcore

open Genome
open TensorFlow

module Phenotype = 
    type Phenotype = { Graph : TFGraph }
    
    let run phenotype = 
        let session = new TFSession(phenotype)
        ""

module SubstrateCompiler =
    type Compile = GenomeData -> Phenotype.Phenotype

    let rec Compile (genome:GenomeData) =
        let mapNode (graph:TFGraph) = function
            //| Input d -> graph.Placeholder(TFDataType.Float, TFShape.Scalar, string d.InnovationNumber)
            | _ ->  graph.Placeholder(TFDataType.Float, TFShape.Scalar, "unknown")

        let mapConnection (graph:TFGraph) conn = graph

        let graph = new TFGraph()
       // let nodes = genome.Nodes |> List.map (fun n -> graph.Constant(42, TFShape.Scalar) )

        {Phenotype.Phenotype.Graph = graph }
