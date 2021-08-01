module MiniCrawler

open System
open System.IO
open System.Net
open System.Text.RegularExpressions

// Get html content from the page.
let getData (url : string) =
    async {
        try 
            use! response = WebRequest.Create(url).AsyncGetResponse()
            
            use stream = response.GetResponseStream()
            use reader = new StreamReader(stream)
            let html = reader.ReadToEnd()   
            return Some html
        with
            | _ -> printfn "fail"
                   return None
    }


// Display data.
let display url (downloaded : Option<string>) = 
    match downloaded with
    | Some html -> printfn "%s   Length: %d symbols" url html.Length
    | _ -> printfn "Fale"

// Get links from html code.
let getLinks (html : string) =
    [for matches in (Regex("<a href\s*=\s*\"?(https?://[^\"]+)\"?\s*>", RegexOptions.Compiled).Matches(html) : MatchCollection) 
    -> matches.Groups.[1].Value]

// Download.
let downloadAll (url : string) =
    let mainData = getData url |> Async.RunSynchronously
    match mainData with
    | Some content -> let links = content |> getLinks
                      links |> List.map (fun link -> getData link) 
                            |> Async.Parallel
                            |> Async.RunSynchronously
                            |> Array.iteri (fun i (result : string option) -> display (links.Item i) result) 
                      
    | None -> ()
    

downloadAll "https://github.com/MihailBeloshapkin"
