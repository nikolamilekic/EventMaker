
module EventMaker.Functions

open System
open System.Text
open System.IO
open System.Globalization
open System.Reflection
open Microsoft.Azure.WebJobs
open Microsoft.Extensions.Configuration
open Microsoft.WindowsAzure.Storage.Blob
open Microsoft.Azure.WebJobs.Extensions.Http
open Microsoft.AspNetCore.Http
open Microsoft.Azure.WebJobs.Hosting
open Microsoft.Azure.WebJobs.Host.Config
open Microsoft.Azure.WebJobs.Description
open MailKit
open MailKit.Net.Imap
open MailKit.Net.Smtp
open MailKit.Search
open MimeKit

open FSharp.Data
open Milekic.YoLo

type Config = {
    FacebookPath : string
    GoogleAccessTokenRequest : HttpRequestBody
    SmtpServer : string * int
    ImapServer : string * int
    EmailCredentials : string * string
    FromName : string
    ToNameAndAddress : string * string
    Subject : string
}

[<AttributeUsage(AttributeTargets.Parameter, AllowMultiple = true)>]
[<Binding>]
type ConfigAttribute() = inherit Attribute()

type ConfigProvider(configuration : IConfiguration) =
    let config = {
        FacebookPath = configuration.["FacebookPath"]
        GoogleAccessTokenRequest =
            [
                "client_secret", configuration.["GoogleClientSecret"]
                "grant_type", "refresh_token"
                "refresh_token", configuration.["GoogleRefreshToken"]
                "client_id", configuration.["GoogleClientId"]
            ] :> seq<_>
            |> HttpRequestBody.FormValues
        SmtpServer =
            configuration.["SmtpServerAddress"],
            configuration.["SmtpServerPort"] |> int
        ImapServer =
            configuration.["ImapServerAddress"],
            configuration.["ImapServerPort"] |> int
        EmailCredentials =
            configuration.["EmailUsername"],
            configuration.["EmailPassword"]
        ToNameAndAddress =
            configuration.["ToName"],
            configuration.["ToAddress"]
        FromName = configuration.["FromName"]
        Subject = configuration.["Subject"]
    }
    interface IExtensionConfigProvider with
        member __.Initialize context =
            context
                .AddBindingRule<ConfigAttribute>()
                .BindToInput(fun _ -> config)
            |> ignore

type Startup() =
    interface IWebJobsStartup with
        member __.Configure builder =
            builder.AddExtension<ConfigProvider>() |> ignore

[<assembly: WebJobsStartup(typedefof<Startup>)>]
do ()

[<Literal>]
let ContactsPath = """PeopleList.json"""
let userAgent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36"

type GoogleAccessToken = JsonProvider<"""{
  "access_token": "ya29.Gls4BnIjINLP7XSfOOSxFc2JPjnxQPTKyMB01lfViP9dnGXGjLUbrjbBzx0lSzRYdkBPKqQXtIuul2PxW5L4aS6GUjQA1PvuNp88ANt-Aw3C5VGW_m1Ou3i6gvOK",
  "scope": "https://www.googleapis.com/auth/contacts.readonly",
  "expires_in": 3600,
  "token_type": "Bearer"
}""">

let getGoogleHeaders config = async {
    let! accessTokenResponse =
        Http.AsyncRequestString (
            "https://www.googleapis.com/oauth2/v4/token",
            body = config.GoogleAccessTokenRequest,
            httpMethod = "POST"
        )
    return
        accessTokenResponse
        |> GoogleAccessToken.Parse
        |> fun t -> t.AccessToken
        |> sprintf "Bearer %A"
        |> fun x -> Seq.singleton ("Authorization", x)
}

type Contacts = JsonProvider<ContactsPath>

let getEventsFromContacts googleHeaders = async {
    let getYear = function | 1604 -> None | y -> Some y

    let! rawContacts =
        Http.AsyncRequestString(
            "https://people.googleapis.com/v1/people/me/connections?personFields=names,birthdays,events&pageSize=2000",
            headers = googleHeaders
        )
    return
        rawContacts
        |> Contacts.Parse
        |> fun r -> r.Connections
        |> Seq.where (fun c -> c.Names |> Seq.isEmpty |> not)
        |> Seq.collect (fun c -> seq {
            let name = c.Names |> Seq.head |> fun n -> n.DisplayName

            yield!
                c.Birthdays
                |> Seq.map (fun b ->
                    let eventName = sprintf "%s's Birthday" name
                    let year = b.Date.Year |> Option.bind getYear
                    eventName, b.Date.Month, b.Date.Day, year)
            yield!
                c.Events
                |> Seq.map (fun e ->
                    let eventName = sprintf "%s's %s" name e.FormattedType
                    let year = e.Date.Year |> Option.bind getYear
                    eventName, e.Date.Month, e.Date.Day, year)
        })
}

let getEventsFromFacebook facebookPath = async {
    let folder (items, name, date) (input : string) =
        let split = input.Split(':')
        if split.Length = 0 then (items, name, date) else
        let command = split |> Seq.head
        let args = split |> Array.skip 1 |> fun args -> String.Join(":", args)
        match command, args with
        | "BEGIN", "VEVENT" -> items, None, None
        | "DTSTART", start -> items, name, Some start
        | "SUMMARY", name -> items, Some name, date
        | "END", "VEVENT" ->
            match name, date with
            | Some n, Some d ->
                let n = n.Replace("birthday", "Birthday")
                let d = DateTime.ParseExact(d, "yyyyMMdd", CultureInfo.InvariantCulture)
                (n, d.Month, d.Day, Option<int>.None) :: items, None, None
            | _ -> items, None, None
        | _ -> (items, name, date)

    let! rawIcs =
        Http
            .AsyncRequestString(
                facebookPath,
                headers = [ "User-Agent", userAgent ])
    let ics = rawIcs.Split([|"\r\n"|], StringSplitOptions.RemoveEmptyEntries)

    let items, _, _ =
        ics
        |> Seq.fold folder ([], None, None)

    return items |> Seq.ofList
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

let run config previous startDate endDate = async {
    let currentTime = DateTime.Now
    let getFutureDate = getFutureDate currentTime
    let! facebookRetriever =
        getEventsFromFacebook config.FacebookPath |> Async.StartChild
    let! googleRetriever =
        async {
            let! googleHeaders = getGoogleHeaders config
            return! getEventsFromContacts googleHeaders
        }
        |> Async.StartChild

    let! fromFacebook = facebookRetriever
    let! fromContacts = googleRetriever
    let previous = getEventsFromPrevious previous

    let complete =
        [ fromContacts; fromFacebook; previous ]
        |> Seq.concat
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

    let tasks =
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
                "- %s @due(%s 20:00) @defer(%s) @tags(Anywhere)"
                name
                formattedDate
                formattedDate)

    return tasks, state
}

let sendTasks config previous (nextState : CloudBlockBlob) = async {
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
    do! nextState.UploadTextAsync(state) |> Async.AwaitTask
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

[<FunctionName("SendTasks")>]
let sendTasksImmediately
    (
        [<HttpTrigger(AuthorizationLevel.Function, "get")>]
            (request : HttpRequest),
        [<Config>] config,
        [<Blob("events/Events.txt", FileAccess.Read)>] previous,
        [<Blob("events/Events.txt", FileAccess.Write)>] nextState
    ) =
    sendTasks config previous nextState |> Async.StartAsTask

[<FunctionName("SendTasksOnSchedule")>]
let sendTasksOnSchedule
    (
        [<TimerTrigger("0 0 0 14 * *")>] (timerInfo : TimerInfo),
        [<Config>] config,
        [<Blob("events/Events.txt", FileAccess.Read)>] previous,
        [<Blob("events/Events.txt", FileAccess.Write)>] nextState
    ) =
    sendTasks config previous nextState |> Async.StartAsTask

[<FunctionName("GetVersion")>]
let getVersion
    (
        [<HttpTrigger(AuthorizationLevel.Function, "get")>]
            (request : HttpRequest)
    ) =
    Assembly.GetExecutingAssembly().GetName().Version.ToString()
