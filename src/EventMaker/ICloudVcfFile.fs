module EventMaker.ICloudVcfFile

open FSharpPlus
open FSharpPlus.Lens
open FParsec

open EventMaker.Data

type State =
    {  ItemData : Map<(int * string), string * string> }
    static member Zero = { ItemData = zero }
module State =
    let inline _itemDataDictionary f s =
        s.ItemData |> f <&> fun v -> { s with ItemData = v }
    let inline _itemData key = _itemDataDictionary << Map._item key

open State

type Property =
    | Name of string
    | Event of string * int option * int * int
    | Generic of string * string * string

let beginCard : Parser<_, State> =
    skipString "BEGIN:VCARD" .>> optional newline >>. setUserState zero
let endCard = skipString "END:VCARD" .>> optional newline
let parameter =
    let actual =
        skipChar ';' >>. many1CharsTill anyChar (followedBy (skipChar ':'))
    opt actual .>> skipChar ':' |>> Option.defaultValue ""
let propertyValue =
    let continuations = skipChar ' ' >>. restOfLine true
    many1Strings2 (restOfLine true) continuations
let genericPropertyRaw =
    many1CharsTillApply anyChar parameter Operators.tuple2 .>>. propertyValue
    |>> fun ((a, b), c) -> a, b, c
let genericProperty = genericPropertyRaw |>> Generic
let name = skipString "FN" >>. parameter >>. propertyValue |>> Name
let date = tuple3 (pint32 .>> skipChar '-') (pint32 .>> skipChar '-') pint32
let bday =
    parameter .>>. date .>> newline
    |>> fun (parameter, (year, month, day)) ->
        let year =
            if parameter.Contains("X-APPLE-OMIT-YEAR") then None else Some year
        year, month, day
let item =
    skipString "item" >>. pint32 .>> skipChar '.'
    .>>. genericPropertyRaw
    .>>. getUserState
    >>= fun ((itemId, (name, parameter, value)), state) ->
        let newState =
            state
            |> _itemData (itemId, name) .-> Some (parameter, value)

        let date =
            newState ^._itemData (itemId, "X-ABDATE")
            |> Option.bind (fun (parameter, value) ->
                match runParserOnString date zero "" value with
                | Success ((y, m, d), _, _) ->
                    let y =
                        if parameter.Contains("X-APPLE-OMIT-YEAR")
                        then None else Some y
                    Some (y, m, d)
                | Failure _ -> None)
        let label = newState ^._itemData (itemId, "X-ABLabel") |> Option.map snd
        let parameter =
            (fun (label : string) (y, m, d) ->
                let label =
                    if label.StartsWith("_$!<") && label.EndsWith(">!$_")
                    then label.Substring(4, label.Length - 8)
                    else label
                    //if label.StartsWith ("_$!") then label.[2..5]
                Event (label, y, m, d))
            <!> label
            <*> date
            |> Option.defaultValue (Generic ($"item{itemId}.{name}", parameter, value))
        setUserState newState >>% parameter

let property =
    notFollowedBy endCard >>. choice [
    name
    skipString "BDAY" >>. bday |>> fun (y, m, d) -> Event ("Birthday", y, m, d)
    skipString "ANNIVERSARY" >>. bday |>> fun (y, m, d) -> Event ("Anniversary", y, m, d)
    item
    genericProperty
]
let contact =
    beginCard >>. many property .>> endCard
    |>> fun properties ->
        let (name, events) =
            properties
            |> List.fold
                (fun (name, events) property ->
                    match property with
                    | Name x -> x, events
                    | Event (a, b, c, d) -> name, (a, b, c, d)::events
                    | _ -> name, events)
                ("", [])
        events
        |> List.map (fun (label, y, m, d) -> {
            Contact = name
            EventName = label
            Month = m
            Day = d
            StartYear = y
        })
let vcfFile = many contact |>> List.concat

let runParserOnString p x =
    match runParserOnString p zero "" x with
    | Success (x, _, _) -> Result.Ok x
    | Failure (e, _, _) -> Result.Error e

let parseEvents = runParserOnString vcfFile
