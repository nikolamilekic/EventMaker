namespace EventMaker.Data

open System
open FSharpPlus
open FSharpPlus.Lens
open Milekic.YoLo

type EventMakerEvent = {
    Contact : string
    EventName : string
    Day : int
    Month : int
    StartYear : int option
}

module EventMakerEvent =
    let inline _contact f s =
        s.Contact |> f <&> fun v -> { s with Contact = v }
    let inline _eventName f s =
        s.EventName |> f <&> fun v -> { s with EventName = v }
    let inline _day f s = s.Day |> f <&> fun v -> { s with Day = v }
    let inline _month f s = s.Month |> f <&> fun v -> { s with Month = v }
    let inline _startYear f s =
        s.StartYear |> f <&> fun v -> { s with StartYear = v }

    let toEventFileEntry x =
        let contact = x^._contact
        let eventName = x^._eventName
        let startYear = x^._startYear
        let month = x^._month
        let day = x^._day
        match startYear with
        | None ->  $"{contact}'s {eventName};{month}/{day}"
        | Some x -> $"{contact}'s {eventName};{month}/{day}/{x}"

type EventInstance = { Event : EventMakerEvent; Date : DateTime }

module EventInstance =
    open EventMakerEvent

    let inline _event f s =
        s.Event |> f <&> fun v -> { s with Event = v }
    let inline _date f s = s.Date |> f <&> fun v -> { s with Date = v }
    let inline _startYear f = _event << _startYear <| f
    let inline _year f =
        let inline _dateTimeYear f (s : DateTime) =
            s.Year |> f <&> fun v -> DateTime(v, s.Month, s.Day)
        _date << _dateTimeYear <| f
    let inline _contactName f = _event << _contact <| f
    let inline _eventName f = _event << _eventName <| f

    let expand (startDate : DateTime) (endDate : DateTime) event =
        let first = {
            Event = event
            Date = DateTime(startDate.Year, event^._month, event^._day)
        }
        let rec inner current = seq {
            yield current
            yield! inner <| over _year ((+) 1) current
        }
        inner first
        |> Seq.skipWhile (fun x -> x^._date < startDate)
        |> Seq.takeWhile (fun x -> x^._date <= endDate)
    let toTaskPaper dueTime extraText x =
        let contact = x^._contactName
        let eventName = x^._eventName
        let startYear = x^._startYear
        let year = x^._year
        let date = x^._date |> fun x -> x.ToString("yyyy-MM-dd")
        let taskName =
            match startYear with
            | None -> $"{contact}'s {eventName}"
            | Some x -> $"{contact}'s {eventName} ({year - x})"
        let extraText = if extraText = "" then "" else $" {extraText}"
        $"- {taskName} @due({date} %s{dueTime}) @defer({date})%s{extraText}"
