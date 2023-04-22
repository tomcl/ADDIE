module SimulationHelpers

open Fable.React
open Fable.React.Props
open Simulation
open CommonTypes



let getDCTable (results: float array) (nodeLst:(Component*int option) list list) =


    let getDCTableLine index  line : ReactElement =
        let index' = index+1
        let nodeNo = 
            if index' < List.length nodeLst then 
                "Node " + string index' + " (V)"
            else " I (A)"
        let nodeComps = 
            if index' < List.length nodeLst then 
                ("",nodeLst[index']) ||> List.fold (fun s (v,i) -> s+"-"+v.Label )
            else ""
        tr [] [
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str nodeNo]
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str line]
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str nodeComps]

        ]

       
    
    let tableFormat =
        [
        colgroup [] [
            col [Style [Width "25%";]]
            col [Style [Width "40%"; WhiteSpace WhiteSpaceOptions.PreLine]]
            col [Style [Width "35%"; WhiteSpace WhiteSpaceOptions.PreLine]]
            ]
        thead [] [
            tr [] [
                th [Style [WhiteSpace WhiteSpaceOptions.Pre]] [str "  Node"]
                th [] [str "Value"]
                th [] [str "Comps"]
            ]
        ]
        ]

    
    let tableLines =
        results
        |> Array.toList
        |> List.indexed
        |> List.collect (fun (i,v) -> [getDCTableLine i (string v)])
    
    let tableChildren = List.append tableFormat tableLines
    if List.length tableLines <> 0 then 
        table 
            [Style 
                [ 
                FontSize "16px"; 
                TableLayout "Fixed"; 
                Width "100%";
                BorderRight "groove";
                BorderLeft "groove"]
                ]
            tableChildren
    else
        table [] []


