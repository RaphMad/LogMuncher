namespace Parsing

open FParsec

module FrqLogfileFormat =

    // exposed types
    type Severity = DEBUG | INFO | WARN | ERROR | FATAL | ALERT | CRITICAL | NOTICE | TRACE

    type LogEntry = { 
        Timestamp: System.DateTime
        Severity: Severity
        HostId: string
        ProcessOrContextId: string
        Title: string
        Message: string
        }

    // common parsers
    let private space : Parser<unit,unit> = skipChar ' '
    let private period = skipChar '.'
    let private colon = skipChar ':'
    let private comma = skipChar ','
    let private textUntil char = manySatisfy (fun c -> c <> char)
    let private separator = skipString "; "

    let private failOnException p = fun stream -> try p stream
                                                  with e -> Reply(Error, messageError e.Message)
    
    // parsers specific to FrqLogfileFormat V1
    module private V1 =

        // time format: 'dd.MM.yyyy HH:mm:ss,000'
        let timestamp = pint32 .>> period >>= fun day ->
                        pint32 .>> period >>= fun month ->
                        pint32 .>> space  >>= fun year ->
                        pint32 .>> colon  >>= fun hour ->
                        pint32 .>> colon  >>= fun minute ->
                        pint32 .>> comma  >>= fun second ->
                        pint32            |>> fun millisecond -> new System.DateTime(year, month, day, hour, minute, second, millisecond)

        let severity  = choice [stringReturn "DEBUG" DEBUG
                                stringReturn "INFO" INFO
                                stringReturn "WARN" WARN
                                stringReturn "ERROR" ERROR
                                stringReturn "FATAL" FATAL]
                        .>> spaces // the trailing spaces are not mentioned in the spec, but inserted by some aplications!

        // format in spec: 'Pdddd', where 'd' is a digit from 0-9
        // however some applications use the invalid 'Tdddd', we therefore simply parse the processId as text
        let processId = textUntil ';'
        let title = textUntil ';'
        let message = restOfLine true

        let logEntry = pipe5 (timestamp .>> separator)
                             (severity  .>> separator)
                             (processId .>> separator)
                             (title     .>> separator)
                              message   <|  fun timestamp severity processId title message -> { Timestamp = timestamp
                                                                                                Severity = severity
                                                                                                HostId = System.String.Empty
                                                                                                ProcessOrContextId = processId
                                                                                                Title = title
                                                                                                Message = message }

        let parse = optional <| skipRestOfLine true >>. // skip optional header line
                    many logEntry

    // parsers specific to FrqLogfileFormat V2 with LifeX specific quirks
    module private V2Lifex =

        // time format in spec:   'YYYY-MM-DDTHH:mm:ss,ssssss+HHmm'
        // but LifeX seems to use 'YYYY-MM-DDTHH:mm:ss,sssZ' - we therefore simply feed the text into System.DateTime.Parse()
        let timestamp =  failOnException (textUntil ';' |>> System.DateTime.Parse)

        let severity  = choice [stringReturn "DEBUG"    DEBUG
                                stringReturn "INFO"    INFO
                                stringReturn "WARN"    WARN
                                stringReturn "ERROR"    ERROR
                                stringReturn "FATAL"    FATAL
                                stringReturn "ALERT"    ALERT
                                stringReturn "CRITICAL" CRITICAL
                                stringReturn "NOTICE"   NOTICE
                                stringReturn "TRACE"    TRACE]
                        .>> spaces // the trailing spaces are not mentioned in the spec, but inserted by some aplications!

        let hostId = textUntil ';'
        let contextId = textUntil ';'
        let title = textUntil ';'

        let endOfMessage = skipChar ';' .>> (skipNewline <|> eof)
        let message = manyCharsTill anyChar (followedBy endOfMessage)

        let logEntry = timestamp .>> separator >>= fun timestamp ->
                       severity  .>> separator >>= fun severity ->
                       hostId    .>> separator >>= fun hostId ->
                       contextId .>> separator >>= fun contextId ->
                       title     .>> separator >>= fun title ->
                       message                 |>> fun message -> { Timestamp = timestamp
                                                                    Severity = severity
                                                                    HostId = hostId
                                                                    ProcessOrContextId = contextId
                                                                    Title = title
                                                                    Message = message }

        let parse = optional <| skipRestOfLine true >>. // skip optional header line
                    sepEndBy logEntry endOfMessage

    let private parseFile filePath parser = match runParserOnFile parser () filePath System.Text.Encoding.UTF8 with
                                            | Success (result, _, _) -> Choice1Of2 result
                                            | Failure (error, _, _)  -> Choice2Of2 error

    // exposed parsing functions
    let public ParseV1 filePath = parseFile filePath V1.parse
    let public ParseV2Lifex filePath = parseFile filePath V2Lifex.parse

    let public Parse filePath = parseFile filePath (attempt V1.parse <|> V2Lifex.parse)
