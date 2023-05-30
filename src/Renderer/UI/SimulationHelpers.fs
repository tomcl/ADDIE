module SimulationHelpers

open Fable.React
open Fable.React.Props
open Simulation
open CommonTypes
open NumberHelpers
open CanvasStateAnalyser
open Fulma


let getDCTable (simDC:DCSimulationResults) simRunning canvasState  =


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

    //printfn "Nodelst : %A" simDC.NodeList
       
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
            let compOption = List.tryFind(fun (c:Component)->c.Id = id) (fst canvasState)
            match compOption with
            |Some comp -> [getCurrentTableLine comp.Label (floatValueToText current)]
            |None -> []
        )
        


    let tableChildren = List.append tableFormat (voltageLines@currentLines)
    if List.length voltageLines <> 0 && simRunning then 
        Table.table []
            tableChildren
    else
        table [] []




let getDCEquations dcSim comps =
    let isResistorType tp =
        match tp with
        |Resistor _ -> true
        |_ -> false

    let findNodeVoltage n =
        if n=0 then 0.
        else dcSim.MNA[n-1]

    let removeZeros no =
        no
        |> string
        |> Seq.rev 
        |> (fun rev-> ("",rev) ||> Seq.fold (fun s v -> 
            if s="" && v='0' then
                s
            else s+ string v        
        ))
        |> Seq.rev
        |> (fun final -> ("",final) ||> Seq.fold (fun s v -> s+string v))
        |> string
    
    match dcSim.MNA with 
    |[||] -> []
    |_ ->
        // voltage sources
        let allDCVoltageSources = findAllVoltageSources comps
        let vsEquations =
            allDCVoltageSources
            |> List.map (fun c->
                match c.Type with
                |VoltageSource (DC v) 
                |VoltageSource (Sine (_,v,_,_)) ->
                    let pair = findNodesOfComp dcSim.NodeList c.Id
                    match pair with
                    |Some (i1,i2) ->
                        if i1=0 then
                            sprintf "V(%i) = %sV" i2 (string v)
                        elif i2=0 then
                            sprintf "V(%i) = %sV" i1 (string v)
                        else
                            sprintf "V(%i) = V(%i) + %sV" i1 i2 (string v)
                    |None -> ""
                |_ -> failwithf "Impossible"
            )
    
        // inductors
        let allInductors = findAllInductors comps
        let inductorEquations =
            allInductors
            |> List.map (fun c->
                let pair = findNodesOfComp dcSim.NodeList c.Id
                match pair with
                |Some (i1,i2) ->
                    if i1=0 then
                            sprintf "V(%i) = 0V" i2
                    elif i2=0 then
                        sprintf "V(%i) = 0V" i1
                    else
                        sprintf "V(%i) = V(%i)" i1 i2        
                |None -> ""
            )

        //potential dividers
        let nodes = List.length dcSim.NodeList
    
        let voltageDividerEquations =
            List.allPairs [0..nodes-1] [0..nodes-1]
            |> List.collect (fun (p1',p2')->
                let p1,p2 = if p1'<p2' then p1',p2' else p2',p1'
                match p1=p2 with
                |true -> []
                |false ->  
                    match findComponentsBetweenNodes (p1,p2) dcSim.NodeList with
                    |[(c,_)] when isResistorType c.Type ->
                        let p3 = 
                            [0..nodes-1] 
                            |> List.removeAt p2
                            |> List.removeAt p1
                            |> List.tryFind (fun i -> 
                            match findComponentsBetweenNodes (p1, i) dcSim.NodeList with
                            |[(res,_)] when isResistorType res.Type -> true
                            |_ ->
                                match findComponentsBetweenNodes (p2, i) dcSim.NodeList with
                                |[(res,_)] when isResistorType res.Type -> true
                                |_ -> false                            
                            )
                        match p3 with
                        |Some node3 ->
                            [p1; p2; node3]
                            |> List.map (fun n -> ((findNodeVoltage n), n))
                            |> List.sortDescending
                            |> (fun x ->
                                match x with
                                |[(_,n1);(_,n2);(_,n3)] -> [(n1,n2,n3)]
                                |_ -> [])
                        |None -> []
                    |_ -> []
                )
            |> List.distinct
            |> List.map (fun (n1,n2,n3)->
                let r1Label,r1Value = 
                    (findComponentsBetweenNodes (n1,n2) dcSim.NodeList) 
                    |> List.head 
                    |> (fun (c,_) -> c.Label,c.Type) 
                    |> (fun (l,tp) -> match tp with |Resistor (v,_) -> l,v |_ -> failwithf "Impossible")
                let r2Label,r2Value = 
                    (findComponentsBetweenNodes (n2,n3) dcSim.NodeList) 
                    |> List.head 
                    |> (fun (c,_) -> c.Label,c.Type) 
                    |> (fun (l,tp) -> match tp with |Resistor (v,_) -> l,v |_ -> failwithf "Impossible")
            
                let frac = removeZeros (r2Value/(r1Value+r2Value))
                sprintf "V(%i) = (%s/(%s+%s))*V(%i) = %s*V(%i)" n2 r2Label r1Label r2Label n1 frac n1        
            )

        (vsEquations @ inductorEquations @ voltageDividerEquations)


let getDCEquationsTable equations =
    let tableFormat =
        [thead [] [
            tr [] [
                th [Style [WhiteSpace WhiteSpaceOptions.Pre]] [str "Equations"]
            ]]]

    let tableChildren = 
        equations
        |> List.map (fun eq ->
            tr [] [
                td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str eq]
            ]
        )

    Table.table []
        (List.append tableFormat tableChildren)
    
    //(vsEquations @ inductorEquations @ voltageDividerEquations)
    
    