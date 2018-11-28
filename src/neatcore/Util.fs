namespace neatcore

module Util =
    open System
    let randomizer min max =
        let random = new System.Random()
        fun () -> 
            let range = max - (min) 
            let scale = random.NextDouble() * range
            scale - min


    let incrementer = 
        let value = ref 0
        fun () -> 
            value := System.Threading.Interlocked.Increment(value)
            !value

    let shuffle xs = xs |> Seq.sortBy (fun _ -> Guid.NewGuid())