module SimulationHelpers

open Fable.React
open Fable.React.Props
open Simulation
open CommonTypes



let getDCTable (results: float array) canvasState (nodeLst:(Component*int option) list list) =


    let getDCTableLine index line : ReactElement =
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

    let getCurrentTableLine resistorLabel value : ReactElement =
        let name = "I(" + resistorLabel + ")"
        tr [] [
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str name]
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str value]
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "Amperes"]

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
    
    let resistors =
        canvasState
        |> fst
        |> List.filter (fun c -> match c.Type with |Resistor _ -> true |_ -> false)
        
    let resistorCurrentLines = 
        resistors
        |> List.map (findNodesOfComp nodeLst)
        |> List.mapi (fun i (n1,n2)->
            let v1 = if n1=0 then 0. else results[n1-1]
            let v2 = if n2=0 then 0. else results[n2-1]
            let resistance = 
                match resistors[i].Type with 
                |Resistor (v,_) -> v 
                |_ -> failwithf "Attempting to find Resistance of a non-Resistor comp"
            (resistors[i].Label,(v2-v1)/resistance)
        )
        |> List.map (fun (r,v)->(r, System.Math.Round (v,4)))
        |> List.collect (fun (r,v) -> [getCurrentTableLine r (string v)])



    let tableChildren = List.append tableFormat (tableLines@resistorCurrentLines)
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


