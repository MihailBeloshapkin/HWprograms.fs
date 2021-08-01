namespace HW2T3

module primeSequence =

    // Generates sequance of prime numbers.
    let primes () =
        let isPrime n =
            seq { 2 .. int(System.Math.Sqrt(float n)) }
            |> Seq.exists (fun x -> n % x = 0)
            |> not
        Seq.initInfinite (fun x -> x + 2)
        |> Seq.filter isPrime