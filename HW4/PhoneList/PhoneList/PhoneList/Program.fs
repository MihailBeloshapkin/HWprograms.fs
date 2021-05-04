module PhoneList

open System

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
    | _ -> ignore

        


process []

//let phoneList = [("Anna", "228"); ("Alexander", "1488"); ("Dmitry", "1337")]
//let result = searchPhoneByName "Alexander" phoneList
    