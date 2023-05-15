module SimulationHelpers

open Fable.React
open Fable.React.Props
open Simulation
open CommonTypes
open NumberHelpers
open Fulma


let getDCTable (simDC:DCSimulationResults) canvasState  =


    let getDCTableLine index value : ReactElement =
        let index' = index+1
        let nodeNo = "V(Node " + string index' + ")"
        tr [] [
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str nodeNo]
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str (value+"V")]
        ]

    let getCurrentTableLine label value : ReactElement =
        let name = "I(" + label + ")"
        tr [] [
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str name]
            td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str (value+"A")]

        ]

       
    let nodesNo = List.length simDC.NodeList-1

    let tableFormat =
        [
        colgroup [] [
            col [Style [Width "50%";]]
            col [Style [Width "50%"; WhiteSpace WhiteSpaceOptions.PreLine]]
            ]
        thead [] [
            tr [] [
                th [Style [WhiteSpace WhiteSpaceOptions.Pre]] [str "Element"]
                th [] [str "Value"]
            ]
        ]
        ]

    
    let voltageLines =
        simDC.MNA
        |> Array.toList
        |> (fun lst ->
            match lst with
            |[] -> [div [] []]
            |_ -> 
                lst
                |> List.removeManyAt (nodesNo) (Array.length simDC.MNA-nodesNo)
                |> List.indexed
                |> List.collect (fun (i,v) -> [getDCTableLine i (floatValueToText v)]))
        
    let currentLines = 
        simDC.ComponentCurrents
        |> Map.toList
        |> List.collect (fun (ComponentId id,current)-> 
            let comp = List.find(fun (c:Component)->c.Id = id) (fst canvasState)
            [getCurrentTableLine comp.Label (floatValueToText current)]
        )
        


    let tableChildren = List.append tableFormat (voltageLines@currentLines)
    if List.length voltageLines <> 0 then 
        Table.table []
            //[Style 
            //    [ 
            //    FontSize "16px"; 
            //    TableLayout "Fixed"; 
            //    Width "100%";
            //    BorderRight "groove";
            //    BorderLeft "groove"]
            //    ]
            tableChildren
    else
        table [] []


