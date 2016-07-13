namespace Parsing

open FParsec

module FrqLogfileFormat =

    // exposed types
    type Severity = DEBUG | INFO | WARN | ERROR | FATAL

    type LogEntry = { 
        Timestamp: System.DateTime
        Severity: Severity
        ProcessId: string
        Title: string
        Message: string
        }
    
    // common parsers
    let space : Parser<unit,unit> = skipChar ' '
    let period = skipChar '.'
    let colon = skipChar ':'
    let comma = skipChar ','
    let separator = skipString "; "
    
    // parsers specific to FrqLogfileFormat V1
    module internal V1 =

        let timestamp = pint32 .>> period >>= fun day ->
                        pint32 .>> period >>= fun month ->
                        pint32 .>> space  >>= fun year ->
                        pint32 .>> colon  >>= fun hour ->
                        pint32 .>> colon  >>= fun minute ->
                        pint32 .>> comma  >>= fun second ->
                        pint32            |>> fun millisecond -> 
                        new System.DateTime(year, month, day, hour, minute, second, millisecond)

        let severity  = choice [ stringReturn "DEBUG" DEBUG
                                 stringReturn "INFO " INFO // the trailing space is not mentioned in the spec, but present in all actual logfiles!
                                 stringReturn "WARN " WARN // the trailing space is not mentioned in the spec, but present in all actual logfiles!
                                 stringReturn "ERROR" ERROR
                                 stringReturn "FATAL" FATAL ]

        let processId = manySatisfy (fun c -> c <> ';') // V1 spec only allows 'P'followed by 4 digits, 
                                                        // but some components seem to use the V2 spec which basically allows any string
        let title = manySatisfy (fun c -> c <> ';')
        let message = restOfLine true

        let logEntry = pipe5 (timestamp .>> separator)
                             (severity  .>> separator)
                             (processId .>> separator)
                             (title     .>> separator)
                             message
                             <| fun timestamp severity processId title message -> { 
                                                                                    Timestamp = timestamp; 
                                                                                    Severity = severity; 
                                                                                    ProcessId = processId; 
                                                                                    Title = title; 
                                                                                    Message = message
                                                                                  }

        let parseLine line = match run logEntry line with
                                | Success (result, _, _) -> Choice1Of2 result
                                | Failure (error, _, _)  -> Choice2Of2 error

    // exposed parsing function
    let ParseV1 filePath = System.IO.File.ReadLines(filePath) |> Seq.map V1.parseLine
