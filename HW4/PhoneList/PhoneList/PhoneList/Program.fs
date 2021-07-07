module PhoneList

open System
open System.IO

// Display cases.
let printCases () =
    printfn "Commands:"
    printfn "1 quit"
    printfn "2 add data"
    printfn "3 search phone by name"
    printfn "4 search name by phone"
    printfn "5 get all data"
    printfn "6 save to file"
    printfn "7 get data from file"


// Add data to list.
let addData name phone phoneList = 
    (name, phone) :: phoneList

// Search phone by name.
let searchPhoneByName name phoneList =
    phoneList |> List.item (phoneList |> List.findIndex (fun x -> fst x = name)) |> snd

// Search name by phone.    
let searchNameByPhone phone phoneList =
    phoneList |> List.item (phoneList |> List.findIndex (fun x -> snd x = phone)) |> fst

// Display data to a console.
let getAllData phoneList =
    let rec loop list =
        match list with
        | head::tail -> 
            printfn "%A %A" (fst head) (snd head)
            loop list.Tail
        | [] -> printfn ""
    loop phoneList


// Save data to path.
let saveDataToFile path phoneList =
    let newList = phoneList |> List.fold (fun acc elem -> 
                                              if acc = "" then (fst elem) + " " + (snd elem)
                                              else acc + "\n" + (fst elem) + " " + (snd elem)) ""
    System.IO.File.WriteAllText(path, newList)

// Load data from file.
let getDataFromFile path =
    File.ReadAllText(path).Split '\n' |> Seq.toList |> List.map (fun (x : string) -> ((x.Split ' ').[0], (x.Split ' ').[1]))

// Start process.
let rec startProcess phoneList =
    printCases ()
    match Console.ReadLine() with
    | "1" -> ignore
    | "2" -> 
        printfn "Input name and number"
        let name = Console.ReadLine()
        let phone = Console.ReadLine()
        startProcess (addData name phone phoneList)
    | "3" -> 
        printfn "Input name:"
        let name = Console.ReadLine()
        printfn "%A" (searchPhoneByName name phoneList)
        startProcess phoneList
    | "4" ->
        printfn "Input phone"
        let phone = Console.ReadLine()
        printf "%A" (searchNameByPhone phone phoneList)
        startProcess phoneList
    | "5" ->
        phoneList |> getAllData
        startProcess phoneList
    | "6" ->
        printfn "Input path to save"
        let path = Console.ReadLine()
        saveDataToFile path phoneList
        startProcess phoneList    
    | "7" ->
        let path = Console.ReadLine()
        path |> getDataFromFile |> startProcess
    | _ -> ignore

        
        
[] |> startProcess