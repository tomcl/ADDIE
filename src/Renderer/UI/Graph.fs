module Graph

open Fable.React
open Fable.React.Props

open Feliz
open Feliz.Plotly

open System
open ModelType
open CommonTypes


let viewGraph (model:Model) =
    match model.CurrentProj,model.showGraphArea with
    | None,_ |Some _,false -> div [] []
    | Some p,true ->
        match model.SimSubTabVisible with
        |ACsim -> 
            let conns = BusWire.extractConnections model.Sheet.Wire
            let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
            let canvasState = comps,conns  
            let outputNode = model.PopupDialogData.ACOutput |> Option.defaultValue "1" |> int
            let ACMag = (Simulation.frequencyResponse canvasState outputNode) |> List.map (fun x -> (log10 x.Mag)*20.)
            let ACPhase = (Simulation.frequencyResponse canvasState outputNode) |> List.map (fun x -> x.Phase*180./Math.PI)
            let freqs = [0.0..0.05..7.0] |> List.map (fun x -> 10.**x)
    
            div [Style [Width "90%"; Float FloatOptions.Left]] [
                        Plotly.plot [
                            plot.traces [
                                traces.scatter [
                                    scatter.x freqs
                                    scatter.y ACMag
                                    scatter.name "Magnitude"
                                ]
                                traces.scatter [
                                    scatter.x freqs
                                    scatter.y ACPhase
                                    scatter.name "Phase"
                                    scatter.yaxis 2
                                    scatter.line [
                                        line.dash.dot
                                        line.width 1
                                        //line.color "blue"
                                    ]
                                ]
                            ]
                            plot.layout [
                                layout.height 350
                                layout.width 1300
                                layout.xaxis [
                                    xaxis.type'.log
                                    xaxis.autorange.true'
                                ]
                                layout.title [
                                    title.text "Magnitude and Phase"
                                ]
                                layout.yaxis [
                                    yaxis.title [
                                        title.text "Magnitude (dB)"
                                    ]
                                    yaxis.autorange.true'
                                    yaxis.zeroline false
                                ]
                                layout.yaxis (2, [
                                    yaxis.title [
                                        title.text "Phase"
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
        
        |TimeSim ->
            let conns = BusWire.extractConnections model.Sheet.Wire
            let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
            let canvasState = comps,conns  
            let inputNode = "2" |> int // model.PopupDialogData.TimeInput |> Option.defaultValue "1" |> int
            let outputNode = model.PopupDialogData.TimeOutput |> Option.defaultValue "1" |> int
            let t,ytr,yss = (Simulation.transientAnalysis canvasState inputNode outputNode)
            let y = [0.;0.] @ List.map2 (fun x y -> x+y) ytr yss
            let t_y = [-t[(List.length t/5)|> int]]@[0.0]@t
            let vs = comps |> List.find (fun c->match c.Type with |VoltageSource _ -> true |_ -> false)
            let x = [0.;0.] @ List.map (Simulation.findInputAtTime (Some vs)) t
            div [Style [Width "90%"; Float FloatOptions.Left]] [
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
        |_ -> div [] []
