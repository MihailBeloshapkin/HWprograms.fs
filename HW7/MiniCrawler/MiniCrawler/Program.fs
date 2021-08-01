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
            return (Some html, url)
        with
            | _ -> printfn "fail"
                   return (None, url)
    }


// Get url and length.
let getUrlAndLength url (downloaded : Option<string>) = 
    match downloaded with
    | Some html -> (url, html.Length)
    | _ -> raise (System.ArgumentNullException("None"))

// Get links from html code.
let getLinks (html : string) =
    [for matches in (Regex("<a href\s*=\s*\"?(https?://[^\"]+)\"?\s*>", RegexOptions.Compiled).Matches(html) : MatchCollection) 
    -> matches.Groups.[1].Value]

// Download.
let downloadAll (url : string) =
    let mainData = getData url |> Async.RunSynchronously |> fst
    match mainData with
    | Some content -> let links = content |> getLinks
                      let data = links |> List.map (fun link -> link |> getData)
                                       |> Async.Parallel
                                       |> Async.RunSynchronously
                      data |> Array.map (fun (c, url) -> getUrlAndLength url c) |> Array.toList
    | None -> []
    
// Display data.
let display (url : string) =
    downloadAll url |> List.map (fun (link, size) -> printfn "%s %d" link size) 

display "https://github.com/MihailBeloshapkin"