namespace Parsing

open FParsec

module FrqLogfileFormat =
    type Severity = DEBUG | INFO | WARN | ERROR | FATAL

    type LogEntry = {
        Timestamp: System.DateTime;
        Severity: Severity;
        ProcessId: int;
        Title: string;
        Message: string;
    }

    let space : Parser<unit,unit> = skipChar ' '
    let period : Parser<unit,unit> = skipChar '.'
    let colon : Parser<unit,unit> = skipChar ':'
    let comma : Parser<unit,unit> = skipChar ','

    let pipe7 p1 p2 p3 p4 p5 p6 p7 fn =
        p1 >>= fun a ->
        p2 >>= fun b ->
        p3 >>= fun c ->
        p4 >>= fun d ->
        p5 >>= fun e ->
        p6 >>= fun f ->
        p7 >>= fun g -> preturn (fn a b c d e f g)

    let timestamp = pipe7 pint32 .>> period // day
                          pint32 .>> period // month
                          pint32 .>> space // year
                          pint32 .>> colon // hour
                          pint32 .>> colon // minute
                          pint32 .>> comma // second
                          pint32
                          (fun day month year hour minute second -> new System.DateTime(day, month, year, hour, minute, second))