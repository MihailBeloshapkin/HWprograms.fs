module PhoneList

open System
open System.IO

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

let g x = x + 1

let f x = x * x

printfn "%A" (1 |> g |> f)

let searchPhoneByName name phoneList =
    phoneList |> List.item (phoneList |> List.findIndex (fun x -> fst x = name)) |> snd
    
let searchNameByPhone phone phoneList =
    phoneList |> List.item (phoneList |> List.findIndex (fun x -> snd x = phone)) |> fst

let getAllData phoneList =
    let rec loop list =
        match list with
        | head::tail -> 
            printfn "%A %A" (fst head) (snd head)
            loop list.Tail
        | [] -> printfn ""
    loop phoneList


let saveDataToFile path phoneList =
    let newList = phoneList |> List.fold (fun acc elem -> acc + "\n" + (fst elem) + " " + (snd elem)) ""
    System.IO.File.WriteAllText(path, newList)

let getDataFromFile path =
    let data = File.ReadAllText(path).Split '\n'
    let listData = Seq.toList data
    List.map (fun (x : string) -> ((x.Split ' ').[0], (x.Split ' ').[1])) listData.Tail


let rec process phoneList =
    printCases ()
    match Console.ReadLine() with
    | "1" -> ignore
    | "2" -> 
        printfn "Input name and number"
        let name = Console.ReadLine()
        let phone = Console.ReadLine()
        process (addData name phone phoneList)
    | "3" -> 
        printfn "Input name:"
        let name = Console.ReadLine()
        printfn "%A" (searchPhoneByName name phoneList)
        process phoneList
    | "4" ->
        printfn "Input phone"
        let phone = Console.ReadLine()
        printf "%A" (searchNameByPhone phone phoneList)
        process phoneList
    | "5" ->
        phoneList |> getAllData
        process phoneList
    | "6" ->
        printfn "Input path to save"
        let path = Console.ReadLine()
        saveDataToFile path phoneList
        process phoneList    
    | "7" ->
        let path = Console.ReadLine()
        path |> getDataFromFile |> process
    | _ -> ignore

        
let start () =
    [] |> process


[] |> process
    