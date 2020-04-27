namespace NeatCore

module Util =
    open System
    let randomizer (random : System.Random) (min : float) (max: float) =
        fun () -> 
            let range = max - (min) 
            let scale = random.NextDouble() * range
            scale - Math.Abs(min)


    let incrementer = 
        fun () ->
            let value = ref 0
            fun () -> 
                value := System.Threading.Interlocked.Increment(value)
                !value

    let shuffle (list: 'a array) = 
        match list with
        | [||] -> Array.empty
        | xs -> xs |> Array.sortBy (fun _ -> Guid.NewGuid())
    
    let randomElement (random:System.Random) (arr: 'a array) =
        let lengthAsFloat = arr.Length |> float
        let number = ((randomizer random 0.0 lengthAsFloat)()) |> int
        arr.[number]

    