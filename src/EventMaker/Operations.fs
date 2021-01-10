
module EventMaker.Functions

open System
open System.Text
open System.IO
open System.Globalization
open System.Reflection
open MailKit
open MailKit.Net.Imap
open MailKit.Net.Smtp
open MailKit.Search
open MimeKit

open FSharp.Data
open Milekic.YoLo

type Config = {
    SmtpServer : string * int
    ImapServer : string * int
    EmailCredentials : string * string
    FromName : string
    ToNameAndAddress : string * string
    Subject : string
}
let getEventsFromPrevious (raw : string) =
    raw.Split("\n", StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun l ->
        let nameDateSplit = l.Split(';')
        let n = nameDateSplit.[0]
        let dateSplit = nameDateSplit.[1].Split('/')
        let m = int dateSplit.[0]
        let d = int dateSplit.[1]
        if dateSplit.Length = 3
        then n, m, d, Some dateSplit.[2] |> Option.map int
        else n, m, d, None)

let getFutureDate (currentTime : DateTime) date =
    if date > currentTime
    then date
    else DateTime(currentTime.Year + 1, date.Month, date.Day)

let run config previous (startDate : DateTime) (endDate : DateTime) = async {
    let currentTime = DateTime.Now
    let getFutureDate = getFutureDate currentTime

    let previous = getEventsFromPrevious previous

    let complete =
        previous
        |> Seq.groupBy (fun (n, m, d, _) -> n, m, d)
        |> Seq.map (fun (_, g) -> g |> Seq.sort |> Seq.last)
        |> Seq.sortBy (fun (_, m, d, _) -> m, d)
        |> List.ofSeq

    let state =
        complete
        |> Seq.map (fun (n, m, d, y) ->
            match y with
            | Some y -> sprintf "%s;%d/%d/%d" n m d y
            | None -> sprintf "%s;%d/%d" n m d)
        |> fun s -> String.Join(Environment.NewLine, s)

    let tasks = seq {
        yield
            sprintf
                "- Script-generated events from %s to %s @autodone(true)"
                (startDate.ToString("yyyy-MM-dd"))
                (endDate.ToString("yyyy-MM-dd"))
        yield!
            complete
            |> Seq.map (fun (n, m, d, y) ->
                let date = DateTime(currentTime.Year, m, d) |> getFutureDate
                let name =
                    match y with
                    | None -> n
                    | Some birthYear ->
                        let age = date.Year - birthYear
                        sprintf "%s (%i)" n age
                name, date)
            |> Seq.where(fun (_, date) -> date >= startDate && date < endDate)
            |> Seq.map (fun (name, date) ->
                let formattedDate = date.ToString("yyyy-MM-dd")
                sprintf
                    "    - %s @due(%s 20:00) @defer(%s) @tags(Anywhere)"
                    name
                    formattedDate
                    formattedDate)
    }

    return tasks, state
}

let sendTasks config previous = async {
    let startDate, endDate =
        let currentTime = DateTime.Now
        let currentYearStart =
            let month = DateTime.Now.Month + 1
            DateTime(currentTime.Year, month, 1)
        let currentYearEnd = currentYearStart.AddMonths 1
        if currentYearEnd < currentTime then
            currentYearStart.AddYears 1,
            currentYearEnd.AddYears 1
        else currentYearStart, currentYearEnd

    let! tasks, state = run config previous startDate endDate
    let tasks = String.Join(Environment.NewLine, tasks)

    if tasks = "" then () else

    let
        {
            SmtpServer = smtpServer
            ImapServer = imapServer
            EmailCredentials = username, password
            FromName = fromName
            ToNameAndAddress = toAddress
            Subject = subject
        } = config

    let from = fromName, username
    let addressFromTuple (t : string * string) = MailboxAddress (fst t, snd t)

    let bodyBuilder = BodyBuilder()

    bodyBuilder.Attachments.Add("Tasks.taskpaper", Encoding.UTF8.GetBytes(tasks))
    |> ignore

    let message = MimeMessage(Subject = subject)
    from |> addressFromTuple |> message.From.Add
    toAddress |> addressFromTuple |> message.To.Add
    message.Body <- bodyBuilder.ToMessageBody()

    do! async {
        use smtpClient = new SmtpClient()
        do! smtpClient.ConnectAsync(fst smtpServer, snd smtpServer) |> Async.AwaitTask
        do! smtpClient.AuthenticateAsync(username, password) |> Async.AwaitTask
        do! smtpClient.SendAsync message |> Async.AwaitTask
        do! smtpClient.DisconnectAsync true |> Async.AwaitTask
    }

    do! async {
        use imapClient = new ImapClient()
        do! imapClient.ConnectAsync(fst imapServer, snd imapServer)  |> Async.AwaitTask
        do! imapClient.AuthenticateAsync(username, password) |> Async.AwaitTask
        let trash = imapClient.GetFolder(SpecialFolder.Trash)
        let sentFolder = imapClient.GetFolder(SpecialFolder.Sent)
        let! _ = sentFolder.OpenAsync(FolderAccess.ReadWrite) |> Async.AwaitTask
        let query = SearchQuery.HeaderContains ("Message-Id", message.MessageId)
        let! messages = sentFolder.SearchAsync query |> Async.AwaitTask
        let! _ = sentFolder.MoveToAsync(messages, trash) |> Async.AwaitTask
        return ()
    }
}
