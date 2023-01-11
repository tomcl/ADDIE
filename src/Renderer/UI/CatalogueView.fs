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
open PopupView
open Sheet.SheetInterface
open DrawModelType
open FilesIO
open Fable.SimpleJson
open Fable.Core.JsInterop
open System
open FileMenuView


let private menuItem styles label onClick =
    Menu.Item.li
        [ Menu.Item.IsActive false; Menu.Item.Props [ OnClick onClick; Style styles ] ]
        [ str label ]

let private createComponent compType label model dispatch =
    Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, compType, label)) |> dispatch

// Anything requiring a standard label should be checked and updated with the correct number suffix in Symbol/Sheet, 
// so give the label ""
let createCompStdLabel comp model dispatch =
    createComponent comp "" model dispatch



let private makeCustom styles model dispatch (loadedComponent: LoadedComponent)  =
    let canvas = loadedComponent.CanvasState
    menuItem styles loadedComponent.Name (fun _ ->
        let custom = Custom {
            Name = loadedComponent.Name
            IOLabels = Extractor.getOrderedCompLabels (IO) canvas
            Form = loadedComponent.Form
            Description = loadedComponent.Description
        }
        
        Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, custom, "")) |> dispatch
    )

let private makeCustomList styles model dispatch =
    match model.CurrentProj with
    | None -> []
    | Some project ->
        // Do no show the open component in the catalogue.
        project.LoadedComponents
        |> List.filter (fun comp -> comp.Name <> project.OpenFileName)
        |> List.filter (fun comp -> 
            match JSHelpers.debugLevel <> 0 with
            |true -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel || comp.Form = Some ProtectedSubSheet)
            |false -> (comp.Form = Some User || comp.Form = Some ProtectedTopLevel)
        )
        |> List.map (makeCustom styles model dispatch)



let private createIOPopup hasInt typeStr (compType:ComponentType) (model:Model) dispatch =
    let title = sprintf "Add %s node" typeStr
    let beforeText =
        fun _ -> str <| sprintf "How do you want to name your %s?" typeStr
    let placeholder = "Component name"
    let beforeInt =
        fun _ -> str <| sprintf "How many bits should the %s node have?" typeStr
    let intDefault = model.LastUsedDialogWidth
    let body = 
        match hasInt with
        | true -> dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch
        | false -> dialogPopupBodyOnlyText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            // TODO: format text for only uppercase and allowed chars (-, not number start)
            // TODO: repeat this throughout this file and selectedcomponentview (use functions)
            let inputText = getText dialogData
            let inputInt = getInt dialogData
            createComponent compType  (formatLabelFromType compType inputText) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) ->
            let notGoodLabel =
                getText dialogData
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> false | _ -> true
            (getInt dialogData < 1) || notGoodLabel
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private createRCLPopup (model:Model) dispatch =
    let title = sprintf "Add a resistor"
    let beforeInt =
        fun _ -> str "Resistance Value (Ohms):"
    let body = dialogPopupBodyNumericalText beforeInt "0 Ohms" dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputInt = getText dialogData
            //printfn "creating adder %d" inputInt
            createCompStdLabel (Resistor (float inputInt)) model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> getInt dialogData < 1
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch



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
                            ldc.CanvasState
                            |> fst
                            |> List.map (fun comp ->
                                match comp.Type with
                                |Custom x when x.Name = sheetName -> 
                                    let newCompType = Custom {x with Description = descrToSave} 
                                    {comp with Type = newCompType}
                                |_ -> comp
                        )
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


/// two react text lines in red
let private twoErrorLines errMsg1 errMsg2 =
    span [Style [Color Red]] [str errMsg1; br []; str errMsg2; br [] ]

/// two line message giving constant value
let private constantValueMessage w (cVal:int64) =
    let mask = 
        if w = 64 then 
            0xffffffffffffffffUL 
        else 
            (1UL <<< w) - 1UL
    let uVal = (uint64 cVal) &&& mask
    let sVal = ((int64 uVal) <<< 64 - w) >>> 64 - w
    let hVal = NumberHelpers.fillHex64 w (int64 uVal)
    let line1 = $"Decimal value: %d{uVal} (%d{sVal} signed)"
    let line2 = $"Hex value: %s{hVal}"
    span [] [str line1; br [] ; str line2; br [] ]


/// check constant parameters and return two react lines with
/// error message or value details
//let parseConstant wMax w cText =
//    if w < 1 || w > wMax then
//            twoErrorLines $"Constant width must be in the range 1..{wMax}" "", None
//    else
//        match NumberHelpers.strToIntCheckWidth w cText with
//        | Ok n ->
//            constantValueMessage w n, Some (Constant1 (w,n,cText))
//        | Error msg ->
//            twoErrorLines msg "", None

//let parseBusCompareValue wMax w cText =
//    if w < 1 || w > wMax then
//            twoErrorLines $"Bus Compare width must be in the range 1..{wMax}" "", None
//    else
//        match NumberHelpers.strToIntCheckWidth w cText with
//        | Ok n ->
//            let n' =
//                if n >= 0 then n |> uint32
//                else
//                    let mask = 
//                        if w = 32 then 
//                            0xffffffffu
//                        else 
//                            (1u <<< w) - 1u
//                    let uVal = (uint32 n) &&& mask
//                    uVal
//            busCompareValueMessage w (uint32 n), Some (BusCompare1 (w,(n'),cText))
//        | Error msg ->
//            twoErrorLines msg "", None

///// create react popup to set a constant
//let private createConstantPopup model dispatch =
//    let title = sprintf "Add Constant" 
//    let beforeInt =
//        fun _ -> str "How many bits has the wire carrying the constant?"
//    let intDefault = 1
//    let parseConstantDialog dialog =
//        parseConstant 64
//            (Option.defaultValue intDefault dialog.Int)
//            (Option.defaultValue "" dialog.Text)
//    let beforeText = (fun d -> div [] [d |> parseConstantDialog |> fst; br [] ])
//    let placeholder = "Value: decimal, 0x... hex, 0b... binary"   
//    let body = dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch
//    let buttonText = "Add"
//    let buttonAction =
//        fun (dialogData : PopupDialogData) ->
//            let width = getInt dialogData
//            let text = Option.defaultValue "" dialogData.Text
//            let constant = 
//                match NumberHelpers.strToIntCheckWidth width text with
//                | Ok n -> n
//                | Error _ -> 0L // should never happen?
//            let text' = if text = "" then "0" else text
//            createCompStdLabel (Constant1(width,constant,text')) model dispatch
//            dispatch ClosePopup
//    let isDisabled = parseConstantDialog >> snd >> Option.isNone
//    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let private makeMenuGroup title menuList =
    details [Open false] [
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

        let muxTipMessage (numBusses:string) = $"Selects the one of its {numBusses} input busses numbered by the value of the select input 
                                to be the output. Adjusts bus width to match"

        let deMuxTipMessage (numBits:string) = $"The output numbered by the binary value 
        of the {numBits} sel inputs is equal to Data, the others are 0"

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
                // TODO
                    makeMenuGroup
                        "Input / Output / Wire"
                        [ 
                          catTip1 "Ground"  (fun _ -> createCompStdLabel Ground model dispatch) "Ground"
                          catTip1 "Output" (fun _ -> createIOPopup true "output" IO model dispatch) "Output connection from current sheet: one or more bits"
                          catTip1 "Wire Label" (fun _ -> createIOPopup false "label" IOLabel model dispatch) "Labels with the same name connect \
                                                                                                                         together wires or busses"]
                    makeMenuGroup
                        "Linear Items"
                        [ 
                          catTip1 "Resistor"  (fun _ -> createRCLPopup model dispatch) "Resistor: PENDING"
                          catTip1 "Capacitor"  (fun _ -> createCompStdLabel (Capacitor 1.) model dispatch) "Capacitor: PENDING"
                          catTip1 "Inductor"  (fun _ -> createCompStdLabel (Inductor 1.) model dispatch) "Inductor: PENDING"
                          catTip1 "Current Source"  (fun _ -> createCompStdLabel (CurrentSource 1.) model dispatch) "Current Source: PENDING"
                          catTip1 "Voltage Source"  (fun _ -> createCompStdLabel (VoltageSource (DC 1.)) model dispatch) "Voltage Source: PENDING"
                          catTip1 "Diode"  (fun _ -> createCompStdLabel Diode model dispatch) "Diode: PENDING"]
                    makeMenuGroup
                        "Non-Linear Items"
                        [ catTip1 "Resistor"  (fun _ -> createRCLPopup model dispatch) "Resistor: PENDING"]

                    makeMenuGroupWithTip styles
                        "This project"
                        "Every design sheet is available for use in other sheets as a custom component: \
                        it can be added any number of times, each instance replicating the sheet logic"
                        (makeCustomList styles model dispatch)

                          
                ]
        (viewCatOfModel) model 
