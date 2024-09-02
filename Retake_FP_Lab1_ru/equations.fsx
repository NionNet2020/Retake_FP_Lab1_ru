let rec dichotomy function a b =
    let midpoint = (a + b) / 2.
    let functionAtA = function(a)
    let functionAtMidpoint = function(midpoint)

    if abs(function(midpoint)) < eps then midpoint
    else if functionAtMidpoint * functionAtA > 0.0 then dichotomy function midpoint b
    else dichotomy function a midpoint

let rec iterations iterationFunction initialGuess =
    if abs (initialGuess - (iterationFunction initialGuess)) < eps then
        initialGuess
    else
        let nextGuess = iterationFunction initialGuess
        iterations iterationFunction nextGuess

let newton function derivative initialGuess =
    let iterationFunction x : float = x - function(x)/derivative(x)
    iterations iterationFunction initialGuess

let f1 x = 2.0 * x * sin x - cos x
let f2 x = System.Math.Exp(x) + (1.0 + System.Math.Exp(2.0 * x))**0.5 - 2.0
let f3 x = System.Math.Log(x) - x + 1.8

let f1_derivative x = 2.0 * sin x + 2.0 * x * cos x + sin x
let f2_derivative x = System.Math.Exp(x) + System.Math.Exp(2.0 * x)/(1.0 + System.Math.Exp(2.0 * x))**0.5
let f3_derivative x = 1.0/x - 1.0

let phi1 x = x - (2.0 * x * sin x - cos x) / (2.0 * sin x + x * cos x)
let phi2 x = x - (System.Math.Exp(x) + (1.0 + System.Math.Exp(2.0 * x))**0.5 - 2.0) / (System.Math.Exp(x) + System.Math.Exp(2.0 * x) / 2.0)
let phi3 x = x - (System.Math.Log(x) - x + 1.8) / (1.0 / x - 1.0)

printfn "func |   dichotomy   |  iterations   |     Newton     "
printfn "====================================================="
printfn "f1   |  %3.10f |  %3.10f |  %3.10f " (dichotomy f1  0.4 1.0) (iterations phi1 0.6) (newton f1 f1_derivative 0.6)
printfn "f2   | %3.10f | %3.10f | %3.10f " (dichotomy f2 -1.0 0.0) (iterations phi2 -0.5) (newton f2 f2_derivative -0.5)
printfn "f3   |  %3.10f |  %3.10f |  %3.10f " (dichotomy f3  2.0 3.0) (iterations phi3 2.5) (newton f3 f3_derivative 2.5)
