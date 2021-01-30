module EventMaker.Program

open System
open System.IO
open Milekic.YoLo
open Argu
open FSharpPlus
open FSharpPlus.Lens

open EventMaker.Data
open EventMakerEvent
open EventInstance

[<NoComparison; NoEquality>]
type Argument =
    | [<ExactlyOnce>] EventsFile of string
    | [<NoAppSettings; Unique>] ICloudContactsFile of string
    | ExtraText of string
    | DueTime of string
    | [<NoAppSettings>] From of string
    | [<NoAppSettings>] Until of string
    | [<NoAppSettings>] Version
    interface IArgParserTemplate with
        member _.Usage = " "

let configFilePath =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.UserProfile),
        ".EventMaker",
        "EventMaker.config")

Directory.CreateDirectory(Path.GetDirectoryName(configFilePath)) |> ignore

[<EntryPoint>]
let main argv =
    printfn
        "EventMaker Version %s (%s)"
        (Metadata.getCallingAssemblyInformationalVersion())
        (DateTimeOffset.Parse(ThisAssembly.Git.CommitDate).ToString("yyyy-MM-dd"))

    try
        let arguments =
            ArgumentParser
                .Create(programName = "EventMaker")
                .Parse(
                    inputs = argv,
                    configurationReader =
                        if File.Exists configFilePath
                        then ConfigurationReader.FromAppSettingsFile configFilePath
                        else ConfigurationReader.NullReader
                )

        if arguments.TryGetResult Version |> Option.isSome then exit 0

        arguments.GetAllResults()
        |> arguments.Parser.PrintAppSettingsArguments
        |> curry File.WriteAllText configFilePath

        let eventsFilePath = arguments.GetResult EventsFile
        let rawEvents = File.ReadAllText(eventsFilePath)
        let iCloudFile = arguments.TryGetResult ICloudContactsFile
        let from =
            arguments.TryPostProcessResult (From, DateTime.Parse)
            |> Option.defaultValue (DateTime.Now)
        let until =
            arguments.TryPostProcessResult (Until, DateTime.Parse)
            |> Option.defaultValue (from.AddMonths(1))
        let extraText = arguments.GetResult(ExtraText, "")
        let dueTime = arguments.GetResult(DueTime, "23:59")

        let result = monad.strict {
            let! eventFileEvents = EventFile.parseEvents rawEvents
            let! iCloudEvents =
                iCloudFile
                |>> (File.ReadAllText >> ICloudVcfFile.parseEvents)
                |> Option.defaultValue (Ok [])
            let events =
                [ eventFileEvents; iCloudEvents ]
                |> Seq.concat |> Seq.distinct |> Seq.toList
            let entries =
                events
                |> Seq.bind (expand from until)
                |> Seq.sortBy (view _date)
                |>> toTaskPaper dueTime extraText
                |>> fun x -> $"    {x}"
            let head =
                sprintf
                    "- Script-generated events from %s to %s @autodone(true)\n"
                    (from.ToString("yyyy-MM-dd"))
                    (until.ToString("yyyy-MM-dd"))
            return (head + (String.concat "\n" entries)), events
        }

        match result with
        | Error x -> printfn $"ERROR: {x}"
        | Ok (entries, events) ->
            printfn $"{entries}"
            let newEventFile =
                events
                |> Seq.sortBy (fun e -> e^._month, e^._day)
                |>> toEventFileEntry

            File.WriteAllLines(eventsFilePath, newEventFile)
        0
    with
        | :? ArguParseException as ex ->
            printfn "%s" ex.Message
            int ex.ErrorCode
        | x ->
            printfn "%s" x.Message
            -1
