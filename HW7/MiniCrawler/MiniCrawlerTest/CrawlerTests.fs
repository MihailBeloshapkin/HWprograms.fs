module MiniCrawlerTest

open NUnit.Framework
open MiniCrawler
open FsUnit


[<Test>]
let ``Simple test`` () =
    downloadAll "https://github.com/MihailBeloshapkin" 
    |> List.map fst |> should equal [ "https://docs.github.com/en/articles/blocking-a-user-from-your-personal-account" 
                                      "https://docs.github.com/en/articles/reporting-abuse-or-spam"
                                      "https://docs.github.com/categories/setting-up-and-managing-your-github-profile" ]
    

