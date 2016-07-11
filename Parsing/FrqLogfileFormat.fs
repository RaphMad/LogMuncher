namespace Parsing

open FParsec

module FrqLogfileFormat =
    type Severity = DEBUG | INFO | WARN | ERROR | FATAL

    type LogEntry = { 
        Timestamp: System.DateTime
        Severity: Severity
        ProcessId: int
        Title: string
        Message: string
        }


    let private space : Parser<unit,unit> = skipChar ' '
    let private period = skipChar '.'
    let private colon = skipChar ':'
    let private comma = skipChar ','

    let private timestamp = pint32 .>> period >>= fun day ->
                    pint32 .>> period >>= fun month ->
                    pint32 .>> space  >>= fun year ->
                    pint32 .>> colon  >>= fun hour ->
                    pint32 .>> colon  >>= fun minute ->
                    pint32 .>> comma  >>= fun second ->
                    pint32            |>> fun millisecond -> 
                    new System.DateTime(year, month, day, hour, minute, second, millisecond)

    let private severity : Parser<Severity, unit>  = choice [stringReturn "DEBUG" DEBUG
                                                             stringReturn "INFO " INFO
                                                             stringReturn "WARN " WARN
                                                             stringReturn "ERROR" ERROR
                                                             stringReturn "FATAL" FATAL]

    let private processId : Parser<int, unit> = skipChar 'P' >>. pint32

    let private title = pstring

    let private message = pstring

    let private sep = skipString "; "

    let private logEntry = tuple5 (timestamp .>> sep)
                                  (severity .>> sep)
                                  (processId .>> sep)
                                  (charsTillString "; " true System.Int32.MaxValue)
                                  (restOfLine true)
                                  |>> fun (t, s, p, ti, m) -> { Timestamp = t; Severity = s; ProcessId = p; Title = ti; Message = m}

    let private parseLine line = match run logEntry line with
                                     | Success (result, _, _) -> Choice1Of2 result
                                     | Failure (error, _, _)  -> Choice2Of2 error

    let parse filePath = System.IO.File.ReadLines(filePath) |> Seq.map parseLine