open System
open System.IO
open System.Collections.Generic
open System.Runtime.Serialization.Formatters // .Binary



let missionsFile = Path.Combine(__SOURCE_DIRECTORY__, ".missions.ms")

type MissionConstructor = string
type CategoryConstructor = string

type Mission = {description: string}
with
    static member CreateNew(m: MissionConstructor) =
        match m with
        | s -> {description = s}

type MissionCategory = {mutable title: string; list: List<Mission>} with
    member this.Title
        with get() = this.title
        and set(s: string) = this.title <- s

    member this.List
        with get() = this.list.AsReadOnly

    member this.RemoveAt(i: int) =
        let len = this.list.Count
        if i < 0 || i >= len then
            false
        else
            this.list.RemoveAt(i); true

    member this.Insert(m: MissionConstructor) =
        match m with
        | s -> this.list.Add(Mission.CreateNew(m))

    member this.MoveUp(i: int) =
        let len = this.list.Count
        if i <= 0 || i >= len then
            false
        else
            let copy : Mission = this.list.[i]
            this.list.[i] <- this.list.[i - 1]
            this.list.[i - 1] <- copy
            true

    member this.MoveDown(i: int) =
        let len = this.list.Count
        if i < 0 || i >= len - 1 then
            false
        else
            let copy : Mission = this.list.[i]
            this.list.[i] <- this.list.[i + 1]
            this.list.[i + 1] <- copy
            true

    static member CreateNew(c: CategoryConstructor) : MissionCategory =
        match c with
        | s -> {title = s; list = new List<Mission>()}
end

type Serializer() = class
    static let s = Binary.BinaryFormatter()
    static member serializer() = s

    static member SaveFile(m: List<MissionCategory>) : unit =
        use stream = new FileStream(missionsFile, FileMode.Create)
        Serializer.serializer().Serialize(stream, m |> box)

    static member LoadFile(filename: string) : List<MissionCategory> =
        use stream = new FileStream(filename, FileMode.Open)
        Serializer.serializer().Deserialize(stream) |> unbox
end

let loadAllFiles() : List<MissionCategory> =
    Serializer.LoadFile(missionsFile)



let matchParameters(args: string[]) =
    match args.[0] with

    // PRINT ALL CATEGORIES AND THEIR CONTAINED MISSIONS
    | "all" ->
        if args.Length <> 1 then
            printfn "this commands takes no parameters."
        else
            let categories : List<MissionCategory> = loadAllFiles()
            let mutable i = 0
            for i in 0..categories.Count-1 do
                printfn "%-4s%s :" (sprintf "%i)" (i+1)) categories.[i].Title
                let lst = categories.[i].List()
                for j in 0..lst.Count-1 do
                    printfn "      %-4s%A" (sprintf "%i)" (j+1)) (lst.Item(j)).description

    // CREATE CATEGORY
    | "create" ->
        if args.Length <> 2 then
            printfn "you must provide 1 argument. See \"help\""
        else
            let categories : List<MissionCategory> = loadAllFiles()
            let cat = MissionCategory.CreateNew(args.[1])
            categories.Add(cat)
            Serializer.SaveFile(categories)

    // DELETE CATEGORY
    | "delete" ->
        // TODO: delete a category
        ()

    // ADD NEW MISSION
    | "new" ->
        if args.Length <> 2 then
            printfn "you must provide 1 argument. See \"help\""
        else
            let categories : List<MissionCategory> = loadAllFiles()
            let index = ref -1
            if Int32.TryParse(args.[1], index) then
                let h = categories.[!index - 1]
                printf "New mission: "
                h.Insert (Console.ReadLine())
                Serializer.SaveFile(categories)
            else
                printfn "Second argument was not a number."

    // MISSION IS DONE
    | "done" ->
        if args.Length <> 3 then
            printfn "you must provide 2 arguments. See \"help\""
        else
            // TODO: might check for indexing out of bounds
            let categories : List<MissionCategory> = loadAllFiles()
            let (index1, index2) = (ref -1, ref -1)
            if Int32.TryParse(args.[1], index1) && Int32.TryParse(args.[2], index2) then
                let cat = categories.[!index1 - 1]
                let success = cat.RemoveAt(!index2 - 1) |> ignore  // TODO: use return value
                Serializer.SaveFile(categories)
            else
                printfn "Second or third argument was not a number."

    // UP-PRIORITIZE A MISSION
    | "up" ->
        if args.Length <> 3 then
            printfn "you must provide 2 arguments. See \"help\""
        else
            // TODO: might check for indexing out of bounds
            let categories : List<MissionCategory> = loadAllFiles()
            let (index1, index2) = (ref -1, ref -1)
            if Int32.TryParse(args.[1], index1) && Int32.TryParse(args.[2], index2) then
                let cat = categories.[!index1 - 1]
                cat.MoveUp(!index2 - 1) |> ignore  // TODO: use return value
                Serializer.SaveFile(categories)
            else
                printfn "Second or third argument was not a number."

    // DOWN-PRIORITIZE A MISSION
    | "down" ->
        if args.Length <> 3 then
            printfn "you must provide 2 arguments. See \"help\""
        else
            // TODO: might check for indexing out of bounds
            let categories : List<MissionCategory> = loadAllFiles()
            let (index1, index2) = (ref -1, ref -1)
            if Int32.TryParse(args.[1], index1) && Int32.TryParse(args.[2], index2) then
                let cat = categories.[!index1 - 1]
                cat.MoveDown(!index2 - 1) |> ignore  // TODO: use return value
                Serializer.SaveFile(categories)
            else
                printfn "Second or third argument was not a number."

    // PRINT THE HELP MENU
    | "help" ->
        // TODO: print usage of this program
        let mutable help = System.Text.StringBuilder()
        let width = System.Console.BufferWidth
        let max (a, b) = if a > b then a else b
        let min (a, b) = if a < b then a else b
        let len = min(80, width)

        let app opt (msg: string) =
            let optTxt = sprintf "%4s%-26s" "" opt
            let optLen = optTxt.Length

            help <- help.Append (optTxt)

            let words: string[] = msg.Split(' ')
            let mutable l = optLen

            for word in words do
                let newLine = l + word.Length >= len

                if newLine then
                    help <- help.Append (sprintf "\n")
                    help <- help.Append (String.init optLen (fun _ -> " "))
                    l <- optLen

                help <- help.Append (word + " ")
                l <- l + word.Length + 1

            help <- help.Append (sprintf "\n")

        help <- help.Append "Usage: missions COMMANDS\n\nAvailable commands:\n"
        app "all" "list all missions"
        app "create TITLE" "create a new category with the given TITLE"
        app "delete NUMBER" "permanently delete the category NUMBER"
        app "new NUMBER" "add a new mission to category NUMBER"
        app "done NUMBER(1) NUMBER(2)" "remove mission NUMBER(2) from category NUMBER(1) permanently"
        app "up NUMBER(1) NUMBER(2)" "up-prioritize mission NUMBER(2) within category NUMBER(1)"
        app "down NUMBER(1) NUMBER(2)" "down-prioritize mission NUMBER(2) within category NUMBER(1)"
        app "help" "show this help message"

        help <- help.Append "\nFound a bug / suggest a feature: <https://github.com/alexpanter/Missions>"

        printf "%s\n" <| help.ToString()

    // INVALID INPUT
    | _ as k ->
        printfn "Instruction not recognized (see \"help\")"



[<EntryPoint>]
let main(args: string[]) =

    // check that missions directory exists
    if not <| File.Exists(missionsFile) then
        use stream = File.Create(missionsFile)
        stream.Close()
        Serializer.SaveFile(new List<MissionCategory>())

    // exit if no arguments were provided
    if args.Length = 0 then
        printfn "No arguments provided (see \"help\")"
        1

    // otherwise run the program
    else
        matchParameters(args)
        0
