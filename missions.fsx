open System
open System.IO
open System.Collections.Generic
open System.Runtime.Serialization.Formatters // .Binary



(****    SERIALIZE TO THIS FILE    ****)
let missionsFile = Path.Combine(__SOURCE_DIRECTORY__, ".missions.ms")



(****    LIST OF CATEGORIES AND THEIR MISSIONS    ****)
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



(****    SERIALIZATION MECHANISMS    ****)
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



(****    UTILITY FUNCTIONS    ****)
let max (a, b) = if a > b then a else b
let min (a, b) = if a < b then a else b




(****    MATCHING COMMAND LINE ARGUMENTS    ****)
let matchArgs(args: string[]) =
    match args.[0] with

    // PRINT ALL CATEGORIES AND THEIR CONTAINED MISSIONS
    | "all" ->
        if args.Length <> 1 then
            printfn "this commands takes no parameters. See \"help\""
        else
            let categories : List<MissionCategory> = loadAllFiles()
            let mutable i = 0
            let width = min(System.Console.WindowWidth, 80)
            let numberLength = 4
            let leftMarginLength = 6 + numberLength
            let mutable msg = System.Text.StringBuilder()

            // print each category
            for i in 0..categories.Count-1 do
                msg <- msg.Append (sprintf "%-4s%s :\n" (sprintf "%i)" (i+1))
                                                        categories.[i].Title)
                let lst = categories.[i].List()

                // print each mission
                for j in 0..lst.Count-1 do
                    msg <- msg.Append (sprintf "      %-4s" (sprintf "%i)" (j+1)))
                    let words: string[] = lst.Item(j).description.Split(' ')
                    let mutable len = leftMarginLength

                    // print each word
                    for word in words do
                        let newLine = len + word.Length >= width

                        if newLine then
                            msg <- msg.Append "\n"
                            msg <- msg.Append (String.init leftMarginLength
                                                           (fun _ -> " "))
                            len <- leftMarginLength

                        msg <- msg.Append (word + " ")
                        len <- len + word.Length + 1

                    msg <- msg.Append "\n"

                //msg <- msg.Append "\n"

            msg.ToString() |> printfn "%s"

    // CREATE CATEGORY
    | "create" ->
        if args.Length <> 1 then
            printfn "this commands takes no parameters. See \"help\""
        else
            let categories : List<MissionCategory> = loadAllFiles()
            printf "New category: "
            let text = Console.ReadLine()
            if text.Length > 0 then
                let cat = MissionCategory.CreateNew(text)
                categories.Add(cat)
                Serializer.SaveFile(categories)
            else
                printfn "Category description was empty.."

    // DELETE CATEGORY
    | "delete" ->
        // TODO: delete a category
        if args.Length <> 2 then
            printfn "you must provide 1 argument. See \"help\""
        else
            let categories : List<MissionCategory> = loadAllFiles()
            let index = ref -1
            let mutable delete = false
            if Int32.TryParse(args.[1], index) then
                if not <| Seq.isEmpty (categories.[!index - 1].List()) then
                    printf "This category contains unfinished missions. Delete? (y/n) "
                    match char <| Console.Read() with
                        | 'y' | 'Y' -> delete <- true
                        | _         -> ()
                    printfn ""
                else delete <- true

            if delete then
                categories.RemoveAt(!index - 1)
                Serializer.SaveFile(categories)

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
                let text = Console.ReadLine()
                if text.Length > 0 then
                    h.Insert (text)
                    Serializer.SaveFile(categories)
                else
                    printfn "Mission description was empty.."
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
        let mutable help = System.Text.StringBuilder()
        let width = System.Console.BufferWidth
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
        app "create" "create a new missions category"
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




(****    MAIN FUNCTION AND ENTRY POINT    ****)
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
        matchArgs(args)
        0
