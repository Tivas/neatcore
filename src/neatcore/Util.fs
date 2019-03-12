namespace neatcore

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

    let shuffle xs = xs |> Seq.sortBy (fun _ -> Guid.NewGuid())