module Main
open Argu
open System
open System.Globalization
open System.Collections.Generic

type CLIArguments =
    | ComputeExpression of string
    | ComputeFile of string
    | FileToDot of expr:string * path:string
    | ExpressionToDot of expr:string * path:string
    | ComputeExpressionToDot of expr:string * path:string
    | ComputeFileToDot of expr:string * path:string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | ComputeExpression _ -> "computes the given expression"
            | ComputeFile _ -> "reads file and compute the expressions in it"
            | FileToDot _ -> "reads file, parse him and write parse tree in dot file"
            | ExpressionToDot _ -> "parses expression and write parse tree"
            | ComputeExpressionToDot _ -> "computes expression and writes parse tree in dot file"
            | ComputeFileToDot _ -> "reads file, compute the expressions in it and writes parse tree in dot file"

open FSharp.Text.Lexing

let parse (text: string) =
    let lexbuf = LexBuffer<char>.FromString text
    let parsed = CalcParser.start Lexer.tokenStream lexbuf   
    parsed

[<EntryPoint>]
let main (argv: string array) =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "Arithmetics interpreter")
    let results = parser.Parse(argv)
    match parser.ParseCommandLine argv with
    | p when p.Contains(ComputeExpression) ->
        let exp = results.GetResult ComputeExpression       
        let program = parse exp
        Interpretator.run program
    | p when p.Contains(ComputeFile) ->
        let exp = results.GetResult ComputeFile       
        let program = parse (System.IO.File.ReadAllText exp) 
        Interpretator.run program
    | p when p.Contains(FileToDot) ->
        let exp, path = results.GetResult FileToDot        
        let program = parse (System.IO.File.ReadAllText exp)
        ParseTree.run false program path
    | p when p.Contains(ExpressionToDot) ->
        let exp, path = results.GetResult ExpressionToDot        
        let program = parse exp
        ParseTree.run false program path
    | p when p.Contains(ComputeFileToDot) ->
        let exp, path = results.GetResult ComputeFileToDot        
        let program = parse (System.IO.File.ReadAllText exp)
        ParseTree.run true program path
    | p when p.Contains(ComputeExpressionToDot) ->
        let exp, path = results.GetResult ComputeExpressionToDot        
        let program = parse exp
        ParseTree.run true program path
    | _ -> parser.PrintUsage() |> printfn "%s"
    0   
