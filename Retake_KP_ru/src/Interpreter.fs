// Interpreter.fs
module Interpreter

open System
open Parser // Подключение модуля Parser для работы с AST

// Определение типов для значений и окружения
type Value =
    | IntVal of int
    | FuncVal of (Value list -> Value)
    | Closure of string list * Expr * Env
and Env = Map<string, Value>

// Функция для вычисления выражений
let rec eval (env: Env) (expr: Expr): Value =
    match expr with
    | Int n -> IntVal n
    | Var x -> 
        match Map.tryFind x env with
        | Some value -> value
        | None -> failwithf "Unbound variable: %s" x
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

// Функция для запуска интерпретатора с исходным кодом
let run_program code =
    let parsed = Parser.parse_code code
    eval builtins parsed
