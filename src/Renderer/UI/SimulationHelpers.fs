module SimulationHelpers

open Fable.React
open Fable.React.Props
open Simulation
open CommonTypes
open ModelType
open DrawModelType
open Sheet.SheetInterface
open NumberHelpers
open CanvasStateAnalyser
open DiagramStyle
open Fulma

/// transforms the DC Simulation results into the DC Table
/// which appears under the DC Sim Subtab. The order is: 
/// (i) node voltages as |V(Node i)| V |
/// (ii) component currents as | I(label) | I |
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

    // node voltages lines 
    let voltageLines =
        simDC.MNA
        |> Array.toList
        |> (fun lst ->
            match lst with
            |[] -> [div [] []]
            |_ -> 
                if nodesNo = List.length lst then
                    lst
                    |> List.indexed
                    |> List.collect (fun (i,v) -> [getDCTableLine i (floatValueToText v)])
                else
                    lst
                    |> List.removeManyAt (nodesNo) (Array.length simDC.MNA-nodesNo)
                    |> List.indexed
                    |> List.collect (fun (i,v) -> [getDCTableLine i (floatValueToText v)]))
    
    // component currents lines
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



/// PENDING FIX
/// getDCEquations function contains bugs which can cause undefined behavior in the circuit
/// it needs fixing before it is re-added to the DCSim runSimulation code
/// probably the error is caused by opamps (3 ports instead of 2)
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
    
    

//---------------------------- RUN SIMULATION FUNCTION --------------------------------//
    
let findInputNodeFromComp (nodeLst: (Component*int option) list list) compId =
    nodeLst |> List.findIndex (List.exists (fun (c,i)->c.Id = compId && i = (Some 0)))

let runSimulation (model:Model) dispatch = 
    let CS = model.Sheet.GetCanvasState ()
    match CS with
    |[],[] -> 
        dispatch ForceStopSim
        dispatch <| SetGraphVisibility false
    |_ ->
        let canvasState = CS |> combineGrounds
        let compsNo,connsNo= List.length (fst canvasState),List.length (snd canvasState)
        match model.PrevCanvasStateSizes = (compsNo,connsNo) with
        |true ->
            match model.Sheet.UpdateSim && model.Sheet.SimulationRunning with
            |false -> ()
            |true -> 
                match checkCanvasStateForErrors canvasState (model.SimSubTabVisible=TimeSim) with
                    |[] ->
                        ClosePropertiesNotification |> dispatch
                        CircuitHasNoErrors |> dispatch
                        SimulationUpdated |> dispatch
                        let res,componentCurrents,nodeLst,dm = Simulation.modifiedNodalAnalysisDC canvasState model.PreviousDiodeModes
                        let equations = [] //getDCEquations model.Sheet.DCSim (fst CS)
                        UpdateVoltages (Array.toList res) |> dispatch
                        UpdateCurrents componentCurrents |> dispatch
                        UpdateDiodeModes dm |> dispatch
                        UpdateDCSim {MNA=res;ComponentCurrents=componentCurrents;NodeList=nodeLst;Equations=equations} |> dispatch
                        
                        match res with
                        |[||] -> 
                            dispatch ForceStopSim
                            dispatch CircuitHasErrors
                            dispatch (SetPropertiesNotification (Notifications.errorPropsNotification "Unknown error in Circuit. Cannot run Simulation")) 
                        |_ ->
                            match model.SimSubTabVisible with
                            |DCsim -> ()
                            |ACsim -> 
                                dispatch UpdateNodes
                                dispatch <| ShowNodesOrVoltagesExplicitState Nodes
                                if model.showGraphArea then
                                    let outputNode = model.SimulationData.ACOutput |> Option.defaultValue "1" |> int
                                    let inputSource = model.SimulationData.ACSource |> Option.defaultValue ""
                                    let res = (frequencyResponse canvasState inputSource outputNode)
                                    UpdateACSim res |> dispatch
                                else ()
                            |TimeSim ->
                                dispatch UpdateNodes
                                dispatch <| ShowNodesOrVoltagesExplicitState Nodes
                                if model.showGraphArea then
                                    let inputSource = model.SimulationData.TimeInput |> Option.defaultValue "VS1" 
                                    let inputNode = inputSource |> findInputNodeFromComp nodeLst
                                    let outputNode = model.SimulationData.TimeOutput |> Option.defaultValue "1" |> int
                                    let timeSim = (transientAnalysis canvasState inputSource inputNode outputNode)
                                    UpdateTimeSim timeSim |> dispatch
                                else ()
                    |err ->
                        dispatch ForceStopSim
                        dispatch CircuitHasErrors
                        dispatch (SetPropertiesNotification (Notifications.errorPropsNotification err[0].Msg))
                        dispatch (Sheet (DrawModelType.SheetT.Msg.Wire (BusWireT.Msg.ErrorWires err[0].ConnectionsAffected) ))
                        dispatch (Sheet (DrawModelType.SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.Msg.ErrorSymbols (err[0].ComponentsAffected,err[0].ComponentsAffected,false) ))))
        |false ->
            dispatch <| UpdateCanvasStateSizes (compsNo,connsNo)
            dispatch ForceStopSim
            //dispatch <| CircuitHasErrors
            dispatch <| SetGraphVisibility false



//---------------------------- SIMULATION SUB-TABS --------------------------------//


let startStopSimDiv model dispatch = 
    let startStopState,startStopStr,startStopMsg = if model.Sheet.SimulationRunning then IsDanger,"Stop",ForceStopSim else if model.Sheet.CanRunSimulation then IsPrimary,"Start",SafeStartSim else IsWarning,"Restart"+CommonTypes.restartSymbol,SafeStartSim
    div [] [
        Heading.h4 [] [str "Start/Stop Simulation"]
        Button.button [
            Button.Color startStopState
            Button.OnClick (fun _-> 
                dispatch startStopMsg
                dispatch <| SetGraphVisibility false
                dispatch <| UpdateNodes
                
                )
        ] [str startStopStr]
        br []
        br []
    ]        

let createACSimSubTab model comps dispatch =
    let sourceOptions =
        comps
        |> List.collect (fun c->
            match c.Type with
            |VoltageSource _ -> [option [Value (c.Id)] [str (c.Label)]]
            |_ -> []
        )

    let outputOptions =
        [1..((List.length model.Sheet.DCSim.NodeList)-1)]
        |> List.collect (fun i ->
            [option [Value (string i)] [str ("Node "+(string i))]]    
        )

    let isDisabled = 
        if model.Sheet.CanRunSimulation && model.Sheet.SimulationRunning then
            match model.SimulationData.ACSource,model.SimulationData.ACOutput with
            |Some x,Some y when x<>"sel" && y<>"sel" -> false
            |_ -> true
        else true

    let magButtonText = if model.SimulationData.ACMagInDB then "dB" else "Normal"
    let freqButtonText = if model.SimulationData.ACFreqInHz then "Hz" else "rads/s"

    div [Style [Margin "20px"]] 
        [ 
            startStopSimDiv model dispatch

            div [Hidden (not model.Sheet.SimulationRunning)] [
            Heading.h5 [] [str "Setup AC Simulation"]
            div [Style [Width "50%"; Float FloatOptions.Left]] [
                Label.label [] [ str "Input" ]
                Label.label [ ]
                    [Select.select []
                    [ select [(OnChange(fun option -> SetSimulationACSource (Some option.Value) |> dispatch))]
                        ([option [Value ("sel")] [str ("Select")]] @ sourceOptions)
                        ]
                    ]
            
                Label.label [] [ str "Output" ]
                Label.label [ ]
                    [Select.select []
                    [ select 
                        [(OnChange(fun option -> SetSimulationACOut (Some option.Value) |> dispatch))]
                        ([option [Value ("sel")] [str ("Select")]] @ outputOptions)
                        ]
                    ]
                ]
            div [Style [Width "50%"; Float FloatOptions.Right]] [
                Label.label [] [ str "Magnitude" ]
                Label.label [ ]
                    [Button.button [Button.OnClick (fun _ -> dispatch SetSimulationACInDB); Button.Color IsLight] [str magButtonText]]
            
                Label.label [] [ str "Frequency" ]
                Label.label [ ]
                    [
                        Button.button [Button.OnClick (fun _ -> dispatch SetSimulationACInHz);Button.Color IsLight] [str freqButtonText] 
                    ]
                ]

            div [Style [Color "White"]] [str "f"]
            br []

            Button.button 
                [   Button.OnClick (fun _ -> 
                        RunSim |> dispatch
                        SetGraphVisibility true |> dispatch); 
                        Button.Color IsPrimary;
                        Button.Disabled isDisabled] 
                [str "Show"]

            ]
            br []
            div [] [str "AC Simulation explores how the circuit behaves over a wide range of frequencies. The plots produced by the AC Simulation represent the magnitude and the phase of the ratio (output_node/input_source). During simulation, all other sources are set to 0 and diodes are assumed to be in conducting mode."]
            
        ]

let createTimeSimSubTab model comps dispatch =
    let sourceOptions =
        comps
        |> List.collect (fun c->
            match c.Type with
            |VoltageSource _ -> [option [Value (c.Id)] [str (c.Label)]]
            |_ -> []
        )

    let outputOptions =
        [1..((List.length model.Sheet.DCSim.NodeList)-1)]
        |> List.collect (fun i ->
            [option [Value (string i)] [str ("Node "+(string i))]]    
        )

    let isDisabled = 
        match model.SimulationData.TimeInput,model.SimulationData.TimeOutput with
        |Some x,Some y when x<>"sel" && y<>"sel" -> 
            match checkTimeSimConditions comps with
            |[] -> false
            |err ->
                true
        |_ -> true

    
    div [Style [Margin "20px"]] 
        [ 
            startStopSimDiv model dispatch
            div [Hidden (not model.Sheet.SimulationRunning)] [
                Heading.h4 [] [str "Setup Time Simulation"]
                Label.label [] [ str "Input" ]
                Label.label [ ]
                    [Select.select []
                    [ select [(OnChange(fun option -> SetSimulationTimeSource (Some option.Value) |> dispatch))]
                        ([option [Value ("sel")] [str ("Select")]] @ sourceOptions)
                        ]
                    ]
            
                Label.label [] [ str "Output" ]
                Label.label [ ]
                    [Select.select [] [ select [(OnChange(fun option -> SetSimulationTimeOut (Some option.Value) |> dispatch))]
                        ([option [Value ("sel")] [str ("Select")]] @ outputOptions)
                        ]
                    ]
                br []
                br []
                Button.button 
                    [   Button.OnClick (fun _ -> 
                            RunSim |> dispatch
                            SetGraphVisibility true |> dispatch); 
                            Button.Color IsPrimary;
                            Button.Disabled isDisabled] 
                    [str "Show"]
            ]
            br []
            div [] [str "Time Simulation explores how the circuit behaves over time using 200 timesteps. The plots produced represent the input and output voltages, along with the two signals (steady-state and transient) that form the output voltage. Time simulation currently supports a maximum of one Voltage Source and one Capacitor or Inductor."]
            

        ]

let createDCSubTab model (comps,conns) dispatch =
    let nodesVoltagesState = match model.Sheet.ShowNodesOrVoltages with |Neither -> IsDanger |Nodes -> IsWarning |Voltages -> IsPrimary
    let currentsState = if model.Sheet.ShowCurrents then IsPrimary else IsDanger
    div [Style [Margin "20px"]] 
        [ 
            let startStopState,startStopStr,startStopMsg = if model.Sheet.SimulationRunning then IsDanger,"Stop",ForceStopSim else if model.Sheet.CanRunSimulation then IsPrimary,"Start",SafeStartSim else IsWarning,"Restart"+CommonTypes.restartSymbol,SafeStartSim
            Heading.h4 [] [str "Start/Stop Simulation"]
            Button.button [
            Button.Color startStopState
            Button.OnClick (fun _-> 
                dispatch startStopMsg
                dispatch <| RunSim
                dispatch <| SetGraphVisibility false
                    )
            ] [str startStopStr]
            br []
            br []
            div [Hidden <| not model.Sheet.SimulationRunning] [
                Heading.h5 [] [str "Adjust on-Canvas Elements"]
                Button.button [Button.OnClick(fun _ -> ShowOrHideCurrents |> dispatch); Button.Color currentsState] [ str "Currents" ]
                span [Style [Width "20px";Color "White"]] [str "asd"]
                Button.button [ 
                Button.OnClick(fun _ -> 
                UpdateNodes |> dispatch
                ShowNodesOrVoltages |> dispatch); Button.Color nodesVoltagesState] [ str "Nodes/Voltages" ]
                                  
                br []
                br []
                Heading.h5 [] [str "DC Results"]
                div [] [

                Menu.menu [Props [Class "py-1";]]  [
                    details [Open true;OnClick (fun _ -> dispatch RunSim)] [
                        summary [menuLabelStyle] [ str "Table Results" ]
                        Menu.list [] [getDCTable model.Sheet.DCSim (model.Sheet.SimulationRunning && model.Sheet.CanRunSimulation) (comps,conns) ]
                    ]
                ]
                
                Menu.menu [Props [Class "py-1";]]  [
                    details [Open false;OnClick (fun _ -> dispatch RunSim)] [
                        summary [menuLabelStyle] [ str "Equations" ]
                        Menu.list [] [(getDCEquationsTable model.Sheet.DCSim.Equations)]
                    ]
                ]

                Menu.menu [Props [Class "py-1";]]  [
                    let paramsDiv =
                        match model.TheveninParams with
                        |None -> null
                        |Some par ->
                            let asstr = (string par.Resistance) + ", " + (string par.Voltage) + ", " + (string par.Current)
                            div [] [
                                Table.table [] [
                                    tr [] [
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "Rth"]
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str ((string (System.Math.Round (par.Resistance,6)))+" "+omegaString)]
                                    ]
                                    tr [] [
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "Vth"]
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str ((string (System.Math.Round (par.Voltage,6)))+" V")]
                                    ]
                                    tr [] [
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "Ino"]
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str ((string (System.Math.Round (par.Current,6)))+" A")]
                                    ]
                                    
                                ]
                                //str asstr
                                
                            ] 

                                
                    details [Open false;] [
                        summary [menuLabelStyle] [ str "Thevenin/Norton" ]
                        Menu.list [] [
                            str "Select a component from the dropdown below to view the thevenin or norton parameters representing the equivalent circuit seen by the component"
                            br []
                            Select.select []
                                [ 
                                let compOptions = 
                                    comps
                                    |> List.filter (fun c->match c.Type with |Opamp |Ground -> false |_ -> true)    
                                    |> List.collect (fun c->[option [Value (c.Id)] [str (c.Label)]])

                                select 
                                    [(OnChange(fun option -> SetSimulationTheveninComp (Some option.Value) |> dispatch))]
                                    ([option [Value ("sel")] [str ("Select")]] @ compOptions)
                                    ]
                            Button.button [Button.Color IsPrimary;Button.OnClick(fun _ -> (dispatch SetSimulationTheveninParams))] [str "Find"]    
                            paramsDiv
                        ]
                    ]
                ]
                    

                //hack to avoid hidden results
                div [Style [Height "100px"]] []
            ]]
        ]


let viewSimSubTab canvasState model dispatch =
    let comps',conns' = combineGrounds canvasState
    //match checkCanvasStateForErrors (comps',conns') with
    //|[] ->
    match model.SimSubTabVisible with
    | DCsim -> 
        createDCSubTab model (comps',conns') dispatch 
    | ACsim -> 
        createACSimSubTab model comps' dispatch
    | TimeSim ->
        createTimeSimSubTab model comps' dispatch