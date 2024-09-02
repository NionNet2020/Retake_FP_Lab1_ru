open System
open FParsec

// Определение типов AST
type Expr =
    | Int of int
    | Var of string
    | Lambda of string list * Expr
    | If of Expr * Expr * Expr
    | Let of (string * Expr) list * Expr
    | Call of Expr * Expr list

// Парсеры для различных выражений
module Parser = 
    let ws = spaces
    let str_ws s = pstring s .>> ws
    let identifier = many1SatisfyL isLetter "identifier" .>> ws

    let int_parser =
        pint32 |>> Int .>> ws

    let var_parser =
        identifier |>> Var

    let expr_list_parser, expr_list_parser_ref = createParserForwardedToRef<Expr, unit>()

    let lambda_parser =
        between (str_ws "(") (str_ws ")")
            (str_ws "lambda" >>. between (str_ws "(") (str_ws ")") (sepBy identifier ws)
             .>>. expr_list_parser)
        |>> (fun (args, body) -> Lambda(args, body))

    let if_parser =
        between (str_ws "(") (str_ws ")")
            (str_ws "if" >>. expr_list_parser .>>. expr_list_parser .>>. expr_list_parser)
        |>> (fun ((cond, then_expr), else_expr) -> If(cond, then_expr, else_expr))

    let let_parser =
        between (str_ws "(") (str_ws ")")
            (str_ws "let" >>. between (str_ws "(") (str_ws ")")
                (sepBy (identifier .>>. expr_list_parser) ws)
             .>>. expr_list_parser)
        |>> (fun (bindings, body) -> Let(bindings, body))

    let call_parser =
        between (str_ws "(") (str_ws ")")
            (expr_list_parser .>>. many expr_list_parser)
        |>> (fun (func, args) -> Call(func, args))

    do expr_list_parser_ref := choice [
        int_parser
        var_parser
        lambda_parser
        if_parser
        let_parser
        call_parser
    ]

    let parse_code code =
        match run expr_list_parser code with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

// Определение типов для значений и окружения
type Value =
    | IntVal of int
    | FuncVal of (Value list -> Value)
    | Closure of string list * Expr * Env
and Env = Map<string, Value>

// Реализация интерпретатора
module Interpreter =

    let rec eval (env: Env) (expr: Expr): Value =
        match expr with
        | Int n -> IntVal n
        | Var x -> Map.find x env
        | Lambda(args, body) -> Closure(args, body, env)
        | If(cond, then_expr, else_expr) ->
            match eval env cond with
            | IntVal 0 -> eval env else_expr
            | _ -> eval env then_expr
        | Let(bindings, body) ->
            let env' = List.fold (fun env' (name, expr) -> Map.add name (eval env expr) env') env bindings
            eval env' body
        | Call(func_expr, arg_exprs) ->
            let func = eval env func_expr
            let args = List.map (eval env) arg_exprs
            match func with
            | Closure(params, body, closure_env) ->
                let env' = List.fold2 (fun env' param arg -> Map.add param arg env') closure_env params args
                eval env' body
            | FuncVal f -> f args
            | _ -> failwith "Not a function"

    // Пример стандартной библиотеки
    let builtins =
        Map.ofList [
            ("add", FuncVal (function
                | [IntVal a; IntVal b] -> IntVal (a + b)
                | _ -> failwith "Invalid arguments to add"))
            ("sub", FuncVal (function
                | [IntVal a; IntVal b] -> IntVal (a - b)
                | _ -> failwith "Invalid arguments to sub"))
            ("mul", FuncVal (function
                | [IntVal a; IntVal b] -> IntVal (a * b)
                | _ -> failwith "Invalid arguments to mul"))
            ("eq", FuncVal (function
                | [IntVal a; IntVal b] -> IntVal (if a = b then 1 else 0)
                | _ -> failwith "Invalid arguments to eq"))
        ]

    // Запуск интерпретатора
    let run_program code =
        let parsed = Parser.parse_code code
        eval builtins parsed

// Основная программа
[<EntryPoint>]
let main argv =
    let program = """
    (let ((fact (lambda (n)
                    (if (eq n 0)
                        1
                        (mul n (fact (sub n 1)))))))
        (fact 5))
    """
    let result = Interpreter.run_program program
    match result with
    | IntVal n -> printfn "Result: %d" n
    | _ -> printfn "Unexpected result"

    0 // Код завершения программы
