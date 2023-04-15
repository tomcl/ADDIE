module Graph

open Fable.React
open Fable.React.Props

open Feliz
open Feliz.Plotly

open System
open ModelType


let viewGraph (model:Model) =
    match model.CurrentProj,model.showGraphArea with
    | None,_ |Some _,false -> null
    | Some p,true ->
        let conns = BusWire.extractConnections model.Sheet.Wire
        let comps = SymbolUpdate.extractComponents model.Sheet.Wire.Symbol
        let canvasState = comps,conns  
        let outputNode = model.PopupDialogData.ACOutput |> Option.defaultValue "1" |> int
        let ACMag = (Simulation.frequencyResponse canvasState outputNode) |> List.map (fun x -> (log10 x.Mag)*20.)
        let ACPhase = (Simulation.frequencyResponse canvasState outputNode) |> List.map (fun x -> x.Phase*180./Math.PI)
        let freqs = [0.0..0.05..7.0] |> List.map (fun x -> 10.**x)
    
        div [Style [Width "90%";]] [
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
                            
                            traces.scatter [
                                scatter.x [ 1; 2; 3; 4 ]
                                scatter.y [ 12; 9; 15; 12 ]
                                scatter.mode [
                                    scatter.mode.lines
                                    scatter.mode.markers
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

