module EventMaker.EventFile

open FSharpPlus
open FParsec
open EventMaker.Data

let date =
    let sepNumber = skipChar '/' >>? pint32
    skipChar ';'
    >>? tuple3 pint32 sepNumber (opt sepNumber)
    .>>? (skipNewline <|> eof)
let eventData =
    skipString "'s" >>? skipChar ' '
    >>? many1CharsTillApply anyChar date (Operators.tuple2)
let entry =
    many1CharsTillApply anyChar eventData <| fun c (e, (m, d, y)) -> {
        Contact = c
        EventName = e
        Month = m
        Day = d
        StartYear = y
    }
let eventFile = many entry
let parseEvents x =
    match run eventFile x with
    | Success (x, _, _) -> Result.Ok x
    | Failure (e, _, _) -> Result.Error e
