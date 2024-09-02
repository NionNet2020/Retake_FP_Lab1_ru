let startX = 0.0
let endX = 1.0
let tolerance = 0.00001
let increment = 0.1

let factorial n =
    let rec factorialHelper acc k =
        if k <= 1 then acc
        else factorialHelper (acc * k) (k - 1)
    factorialHelper 1 n

let naiveTaylorSeries x tolerance =
    let rec calculateNaiveTerm n acc =
        let sign = if n % 2 = 0 then -1.0 else 1.0
        let twoN = 2 * n
        let denominator = float(factorial twoN)
        let newAcc = sign * (2.0 ** (float(twoN - 1))) * (x ** (float(twoN))) / denominator + acc
        if abs (newAcc - acc) <= tolerance then (acc, n)
        else calculateNaiveTerm (n + 1) newAcc
    calculateNaiveTerm 1 0.0

let smartTaylorSeries x tolerance =
    let rec calculateSmartTerm n previousTerm acc =
        let twoN = 2 * n
        let currentTerm = -1.0 * previousTerm * 2.0 ** 2.0 * x ** 2.0 / ((float(twoN) - 1.0) * float(twoN))
        let newAcc = acc + currentTerm
        if abs (newAcc - acc) <= tolerance then (acc, n)
        else calculateSmartTerm (n + 1) currentTerm newAcc

    let firstTerm = 2.0 * x ** 2.0 / float(factorial 2)
    calculateSmartTerm 2 firstTerm firstTerm

let printTable startX endX tolerance =
    printfn " x |     f(x)     | Naive Taylor | term # | Smart Taylor | term # "
    printfn "===|==============|==============|========|==============|========"

    let rec printRow (x: float) =
        if x >= endX then ()
        else
            let fx = sin x ** 2.0
            let (naiveResult, naiveTerms) = naiveTaylorSeries x tolerance
            let (smartResult, smartTerms) = smartTaylorSeries x tolerance
            printfn "%2.1f| %1.10f | %1.10f | %6d | %1.10f | %6d " x fx naiveResult naiveTerms smartResult smartTerms
            printRow (x + increment)

    printRow startX

    printfn "------------------------------------------------------------------"

printTable startX endX tolerance
