// Learn more about F# at http://fsharp.org

open System
open Argu

type LoggerArgs =
  | [<MainCommand; ExactlyOnce; Last>] Entry of entry:string
  | [<AltCommandLine("-t")>] Tag of tag:string option
  | [<EqualsAssignment>] File of path:string option
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Entry _ -> "entry to be logged"
      | Tag _ -> "entry tag"
      | File _ -> "where to log to"

module Logging =

  type LogCommand = {
    entry       : string
    tag         : string option
    filePath    : string option
  }

  [<RequireQualifiedAccess>]
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module LogCommand =

    let zero = { entry = null; tag = None; filePath = None }

    let fromLoggerArgs (loggerArgs:LoggerArgs list) =
      loggerArgs
      |> List.fold(fun acc arg ->
        match arg with
        | Entry e    -> { acc with entry = e }
        | Tag t      -> { acc with tag = t }
        | File p     -> { acc with filePath = p }
      ) zero

  [<RequireQualifiedAccess>]
  module Logger =

    let private now() = System.DateTime.UtcNow.ToString("u")

    let log output cmd =
      let tag =
        match cmd.tag with
        | None -> ""
        | Some t -> sprintf "[%s]" t
      let msg = sprintf "[%s]%s %s\n" (now()) tag cmd.entry
      output msg

  [<RequireQualifiedAccess>]
  module FileLogger =

    open System.IO

    let log file msg =
      if not <| File.Exists file then
        File.Create file |> ignore
      File.AppendAllText(file, msg)

  module WorkLogger =

    let private workLog = "work.log"

    let private useDefaultFile() =
      Environment.CurrentDirectory <- "/Users/williamczifro/.work-logger/"
      workLog

    let log args =
      let cmd = LogCommand.fromLoggerArgs args
      let file =
        match cmd.filePath with
        | Some p -> p
        | _ -> useDefaultFile()
      let log' = Logger.log <| FileLogger.log file

      log' cmd

[<EntryPoint>]
let main argv =
  let errorHandler =
    ProcessExiter(
      colorizer = function ErrorCode.HelpText -> None
                         | _ -> Some ConsoleColor.Red
    )
  let parser =
    ArgumentParser.Create<LoggerArgs>(
      programName = "workLogger",
      errorHandler = errorHandler
    )

  try
    let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
    Logging.WorkLogger.log <| results.GetAllResults()
  with
  | ex -> printfn "%s" ex.Message

  0
