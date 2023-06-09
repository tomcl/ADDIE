module Graph

open Fable.React
open Fable.React.Props

open Feliz
open Feliz.Plotly

open System
open ModelType
open CommonTypes
open Simulation
open Fulma


let viewGraph (model:Model) dispatch =
    match model.CurrentProj,model.showGraphArea with
    | None,_ |Some _,false -> [div [] []]
    | Some p,true ->
        match model.SimSubTabVisible with
        |ACsim -> 
            //let conns = BusWire.extractConnections model.Sheet.Wire
            //let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
            //let canvasState = comps,conns  
            //let outputNode = model.PopupDialogData.ACOutput |> Option.defaultValue "1" |> int
            let dBOrNot x = if model.SimulationData.ACMagInDB then (log10 x.Mag)*20. else x.Mag
            let HzOrOmega x = if model.SimulationData.ACFreqInHz then x/(2.*Math.PI) else x


            let ACMag = model.Sheet.ACSim |> List.map (dBOrNot)
            let ACPhase = model.Sheet.ACSim |> List.map (fun x -> x.Phase*180./Math.PI)
            let freqs = [0.0..0.05..7.0] |> List.map (fun x -> 10.**x) |> List.map (HzOrOmega)
            let graphTitle = sprintf "Magnitude and Phase (Node %s/%s)" (Option.defaultValue "" model.SimulationData.ACOutput) (CanvasStateAnalyser.findLabelFromId (SymbolUpdate.extractComponents model.Sheet.Wire.Symbol) (Option.defaultValue "" model.SimulationData.ACSource))
            [
                div [Style [Width "80%"; Float FloatOptions.Left]] [
                        Plotly.plot [
                            plot.traces [
                                traces.scatter [
                                    scatter.x freqs
                                    scatter.y ACMag
                                    scatter.name ("Mag(Node "+(string model.SimulationData.ACOutput)+")")
                                ]
                                traces.scatter [
                                    scatter.x freqs
                                    scatter.y ACPhase
                                    scatter.name ("Phase(Node "+(string model.SimulationData.ACOutput)+")")
                                    scatter.yaxis 2
                                    scatter.line [
                                        line.dash.dot
                                        line.width 1
                                        //line.color "blue"
                                    ]
                                    //scatter.visible.legendonly 
                                ]
                            ]
                            plot.layout [
                                layout.height 350
                                layout.width 1300
                                layout.xaxis [
                                    xaxis.type'.log
                                    xaxis.autorange.true'
                                    xaxis.title [
                                        title.text (if model.SimulationData.ACFreqInHz then "Frequency (Hz)" else "Frequency (rads/s)")
                                    ]
                                ]
                                layout.title [
                                    title.text graphTitle
                                ]
                                layout.yaxis [
                                    yaxis.title [
                                        title.text (if model.SimulationData.ACMagInDB then "Magnitude (dB)" else "Magnitude")
                                    ]
                                    yaxis.autorange.true'
                                    yaxis.zeroline false
                                ]
                                layout.yaxis (2, [
                                    yaxis.title [
                                        title.text ("Phase("+degreesSymbol+")")
                                        title.font [
                                            font.color (color.rgb(148, 103, 189))
                                        ]
                                    ]
                                    yaxis.autorange.true'
                                    yaxis.tickfont [
                                        tickfont.color (color.rgb(148, 103, 189))
                                    ]
                                    yaxis.zeroline false
                                    yaxis.overlaying.y 1
                                    yaxis.side.right
                                ])
                        ]
                        ]
                        ]
                div [Style [Width "20%"; Float FloatOptions.Left; Padding "20px"]] [Delete.delete [ Delete.Size IsMedium; Delete.OnClick(fun _ -> SetGraphVisibility false |> dispatch) ] [ ]]
            ]
        
        |TimeSim ->
            //let conns = BusWire.extractConnections model.Sheet.Wire
            let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
            let extractTimeSimResults res =
                (res.TimeSteps,res.Transient,res.SteadyState)
            let extractTimeSimParameters res =
                (res.Tau,res.Alpha,res.HFGain,res.DCGain)
            //let canvasState = comps,conns  
            //let inputNode = "2" |> int // model.PopupDialogData.TimeInput |> Option.defaultValue "1" |> int
            //let outputNode = model.PopupDialogData.TimeOutput |> Option.defaultValue "1" |> int
            let t,ytr,yss = extractTimeSimResults model.Sheet.TimeSim
            let tau,alpha,hf,dc = extractTimeSimParameters model.Sheet.TimeSim
            match t,ytr,yss with
            |[],[],[] -> [div [] []]
            |_ ->
                let y = 
                    if ytr = [] then [0.;0.] @ yss
                    else [0.;0.] @ List.map2 (fun x y -> x+y) ytr yss
                let t_y = [-t[(List.length t/5)|> int]]@[0.0]@t
                let vs = comps |> List.find (fun c->match c.Type with |VoltageSource _ -> true |_ -> false)
                let x = [0.;0.] @ List.map (Simulation.findInputAtTime (Some vs)) t
                [
                    div [Style [Width "80%"; Float FloatOptions.Left]] [
                            Plotly.plot [
                                plot.traces [
                                    traces.scatter [
                                        scatter.x t_y
                                        scatter.y y
                                        scatter.name "y"
                                    ]
                                    traces.scatter [
                                        scatter.x t
                                        scatter.y yss
                                        scatter.name "y_ss"
                                        scatter.line [
                                        line.dash.dot
                                        line.width 1
                                        line.color "green"]
                                        ]
                                    traces.scatter [
                                        scatter.x t
                                        scatter.y ytr
                                        scatter.name "y_tr"
                                        scatter.line [
                                            line.dash.dot
                                            line.width 1
                                            line.color "red"
                                        ]
                                    ]
                                    traces.scatter [
                                        scatter.x t_y
                                        scatter.y x
                                        scatter.name "x"
                                        scatter.line [
                                        line.color "black"
                                        ]
                                    ]
                            
                                ]
                                plot.layout [
                                    layout.height 350
                                    layout.width 1300
                                    layout.title [
                                        title.text "Time Analysis"
                                    ]
                                    layout.yaxis [
                                        yaxis.title [
                                            title.text "Voltage (V)"
                                        ]
                                        yaxis.autorange.true'
                                        yaxis.zeroline false
                                    ]
                            ]
                            ]
                            ]
                    div [Style [Width "20%"; Float FloatOptions.Left; Padding "20px"]] 
                        [   
                            div [Style [MarginLeft "auto"; MarginRight "0"; Float FloatOptions.Right]] [ 
                                Delete.delete  [Delete.Size IsMedium; Delete.OnClick(fun _ -> SetGraphVisibility false |> dispatch) ] [ ]
                            ]
                            Heading.h5 [] [str "Transient Parameters"]
                            Table.table [] [
                                tr [] [
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "tau"]
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str (string tau)]
                                ]
                                tr [] [
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "Transient Amplitude"]
                                    td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str (string alpha)]
                                ]
                                tr [] [
                                        td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "HFGain"]
                                        td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str (string hf)]
                                ]
                                tr [] [
                                        td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str "DCGain"]
                                        td [Style [Color "Black"; VerticalAlign "Middle"; WhiteSpace WhiteSpaceOptions.Pre]] [str (string dc)]
                                ]
                            
                            ]
                        ]//[Button.button [Button.OnClick(fun _ -> SetGraphVisibility false |> dispatch)] [str "X"]]
                
                ]
        |_ -> 
            SetGraphVisibility false |> dispatch
            [div [] []] 
