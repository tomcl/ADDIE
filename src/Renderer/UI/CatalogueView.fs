(*
    CatalogueView.fs

    View for catalogue in the right tab.
*)

module CatalogueView

open Fulma
open Fulma.Extensions.Wikiki
open Fable.React
open Fable.React.Props
open DiagramStyle
open ModelType
open ModelHelpers
open CommonTypes
open NumberHelpers
open PopupView
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open Fable.SimpleJson
open Fable.Core.JsInterop
open System
open FileMenuView
open ComponentCreation

let private menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]

let createSheetDescriptionPopup (model:Model) previousDescr sheetName dispatch =
    let title = sprintf "Sheet Description"
    let beforeText =
        fun _ -> str <| sprintf "Add description for sheet '%s'" sheetName
    let body =  dialogPopupBodyOnlyTextWithDefaultValue beforeText "Description" previousDescr dispatch
    let buttonText = "Save"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let descr = getText dialogData
            let descrToSave =
                match descr with
                |"" -> None
                |_ -> Some descr
            
            match model.CurrentProj with
            |None -> failwithf "Can't happen"
            |Some p ->
                let target_ldc = p.LoadedComponents |> List.find (fun x -> x.Name = sheetName)
                let other_ldc = p.LoadedComponents |> List.filter (fun x -> x.Name <> sheetName)
                let target_ldc' = {target_ldc with Description=descrToSave}  //add description to ldc
                
                let other_ldc' =  //find all custom comps originating from that sheet and update their description
                    other_ldc 
                    |> List.map (fun ldc -> 
                        let newComps = 
                            ldc.CanvasState |> fst
                            
                        let newCS = newComps,(ldc.CanvasState |> snd)
                        {ldc with CanvasState = newCS}
                    )

                let fixed_ldcs = other_ldc'@[target_ldc'] 
                let p' = {p with LoadedComponents=fixed_ldcs}
                let model' = {model with CurrentProj = Some p'}
                dispatch <| SetProject p'
                saveOpenFileActionWithModelUpdate model' dispatch |> ignore
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            false  //allow all
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private makeMenuGroup title menuList =
    details [Open false;] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]


let mutable firstTip = true

let mutable tippyNodes: Browser.Types.Element list = []

let private makeMenuGroupWithTip styles  title tip menuList =
    details [
        Open false;
        HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
        Tooltip.dataTooltip tip
        Style styles
    ] [
        summary [menuLabelStyle] [ str title ]
        Menu.list [] menuList
    ]

let compareModelsApprox (m1:Model) (m2:Model) =

    let m1r = reduceApprox m1
    let m2r = reduceApprox m2
    let b = m1r = m2r
    //printfn "Model equality:%A" b
    //if b = false then printfn "\n\n%A\n\n%A\n\n" m1r m2r
    b




let viewCatalogue model dispatch =

        let viewCatOfModel = fun model ->                 
            let styles = 
                match model.Sheet.Action with
                | SheetT.InitialisedCreateComponent _ -> [Cursor "grabbing"]
                | _ -> []

            let catTip1 name func (tip:string) = 
                let react = menuItem styles name func
                div [ HTMLAttr.ClassName $"{Tooltip.ClassName} {Tooltip.IsMultiline}"
                      Tooltip.dataTooltip tip
                      Style styles
                    ]
                    [ react ]
            Menu.menu [Props [Class "py-1"; Style styles]]  [
                    makeMenuGroup
                        "Ground"
                        [ 
                          catTip1 "Ground"  (fun _ -> createCompStdLabel Ground model dispatch) "Ground"]
                    makeMenuGroup
                        "Voltage / Current Sources"
                        [ 
                          catTip1 "Voltage Source"  (fun _ -> createVSPopup model (VoltageSource (DC 0)) dispatch) "Voltage Source"
                          catTip1 "Current Source"  (fun _ -> createRCLIPopup model (CurrentSource (0.,"0")) dispatch) "Current Source"]

                    makeMenuGroup
                        "Passive Elements"
                        [ 
                          catTip1 "Resistor"  (fun _ -> createRCLIPopup model (Resistor (0,"0")) dispatch) "Resistor"
                          catTip1 "Capacitor"  (fun _ -> createRCLIPopup model (Capacitor (0,"0"))dispatch) "Capacitor"
                          catTip1 "Inductor"  (fun _ -> createRCLIPopup model (Inductor (0,"0")) dispatch) "Inductor"]
                    
                    makeMenuGroup
                        "Amplifier / Diodes"
                        [ 
                          catTip1 "Operational Amplifier"  (fun _ -> createCompStdLabel Opamp model dispatch) "Operational Amplified"
                          catTip1 "Linearized Diode"  (fun _ -> createCompStdLabel DiodeL model dispatch) "Diode: This is a linearized equivalent diode that assumes a 0.7 constant voltage drop on conducting mode"
                          catTip1 "Real Diode"  (fun _ -> createCompStdLabel DiodeR model dispatch) "Real Diode"]
                ]
        (viewCatOfModel) model 
