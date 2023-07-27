(*
    SelectedComponentView.fs

    View for the selected component in the right tab.
*)

module SelectedComponentView

open Fulma
open Fable.React
open Fable.React.Props
open Fulma.Extensions.Wikiki
open Feliz


open JSHelpers
open ModelType
open CommonTypes
open PopupView
open Notifications
open Sheet.SheetInterface
open DrawModelType
open CatalogueView
open FileMenuView
open NumberHelpers

module Constants =
    let labelUniqueMess = "Components must have a unique label within one sheet"


let private readOnlyFormField name body =
    Field.div [] [
        Label.label [] [ str name ]
        body
    ]



let private textValueFormField isRequired name defaultValue isBad onChange =        
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ 
                Id "labelInputElement"; 
                OnPaste preventDefault; 
                SpellCheck false; 
                Name name; 
                AutoFocus true; 
                Style [ Width "200px"]; ]
            Input.DefaultValue defaultValue
            Input.CustomClass "www"
            Input.Placeholder (if isRequired then "Value (required)" else "Value    (optional)")
            Input.OnChange (getTextEventValue >> onChange)
        ]
        br []
        span [Style [FontStyle "Italic"; Color "Red"]; Hidden (isBad = None)] [str <| Option.defaultValue "" isBad]
    ]


let private textFormField isRequired name defaultValue isBad onChange onDeleteAtEnd =
    let onDelete (ev: Browser.Types.KeyboardEvent) =
        if ev.key = "Delete" then  
            let textEl: Browser.Types.HTMLInputElement = unbox (Browser.Dom.document.getElementById "labelInputElement")
            let length = textEl.value.Length
            let start = textEl.selectionStart
            if length = start then
                // Delete pressed at end of input box should go to draw block as
                // a single component DELETE action - since that was probably wanted.
                // NB it will only happen if just one component is highlighted
                onDeleteAtEnd()
            
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ 
                Id "labelInputElement"; 
                OnPaste preventDefault; 
                SpellCheck false; 
                Name name; 
                AutoFocus true; 
                Style [ Width "200px"]; 
                OnKeyDown onDelete]
            Input.DefaultValue defaultValue
            Input.CustomClass "www"
            Input.Placeholder (if isRequired then "Name (required)" else "Name (optional)")
            Input.OnChange (getTextEventValue >> onChange)
        ]
        br []
        span [Style [FontStyle "Italic"; Color "Red"]; Hidden (isBad = None)] [str <| Option.defaultValue "" isBad]
    ]

let private textFormFieldSimple name defaultValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.text [
            Input.Props [ OnPaste preventDefault; SpellCheck false; Name name; AutoFocus true; Style [ Width "200px"]]
            Input.DefaultValue defaultValue
            Input.Type Input.Text
            Input.OnChange (getTextEventValue >> onChange)
        ] 
    ]


let private intFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%d" defaultValue
            Input.OnChange (getIntEventValue >> onChange)
        ]
    ]

let private floatFormField name (width:string) defaultValue minValue onChange =
    Field.div [] [
        Label.label [] [ str name ]
        Input.number [
            Input.Props [Style [Width width]; Min minValue]
            Input.DefaultValue <| sprintf "%A" defaultValue
            Input.OnChange (getFloatEventValue >> onChange)
        ]
    ]


let private makeScaleAdjustmentField model (comp:Component) dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let textw =  
        match comp.SymbolInfo with
        |Some si ->
            match si.HScale with
            |Some no -> no
            |None -> 1.0
        |None -> 1.0
    let texth =  
        match comp.SymbolInfo with
        |Some si ->
            match si.VScale with
            |Some no -> no
            |None -> 1.0
        |None -> 1.0

    div [] [
        floatFormField "Width Scale" "60px" textw 0.0 (
            fun (newWidth) ->
                if newWidth < 0.0
                then
                    let props = errorPropsNotification "Invalid number of bits."
                    dispatch <| SetPropertiesNotification props
                else
                    model.Sheet.ChangeScale sheetDispatch (ComponentId comp.Id) newWidth Horizontal
                    dispatch ClosePropertiesNotification
        )
        floatFormField "Height Scale" "60px" texth 0.0 (
            fun (newWidth) ->
                if newWidth < 0.0
                then
                    let props = errorPropsNotification "Invalid number of bits."
                    dispatch <| SetPropertiesNotification props
                else
                    model.Sheet.ChangeScale sheetDispatch (ComponentId comp.Id) newWidth Vertical
                    dispatch ClosePropertiesNotification
        )
    ]



let mockDispatchS msgFun msg =
    match msg with
    | Sheet (SheetT.Msg.Wire (BusWireT.Msg.Symbol sMsg)) ->
        msgFun msg
    | _ -> ()
/// Return dialog fileds used by constant, or default value

let msgToS = 
    BusWireT.Msg.Symbol >> SheetT.Msg.Wire >> Msg.Sheet

let constantDialogWithDefault (w,cText) dialog =
    let w = Option.defaultValue w dialog.Int
    let cText = Option.defaultValue cText dialog.Text
    w, cText

let private makeSliderField model (comp:Component) text dispatch  =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let onchange = (
        fun newValue ->
            if textToFloatValue newValue = None
            then
                let props = errorPropsNotification "Invalid number value"
                dispatch <| SetPropertiesNotification props
            else
                model.Sheet.ChangeRLCIValue sheetDispatch (ComponentId comp.Id) (Option.get (textToFloatValue newValue)) (floatValueToText (Option.get (textToFloatValue newValue)))
                //SetComponentLabelFromText model comp text' // change the JS component label
                let lastUsedWidth = model.LastUsedDialogWidth 
                dispatch (ReloadSelectedComponent (lastUsedWidth)) // reload the new component
                dispatch <| SetPopupDialogText (Some newValue)
                //sheetDispatch <| SheetT.CanvasChanged
                dispatch ClosePropertiesNotification
    )

    let extractedValue =
        match comp.Type with
        | Resistor (v,_) | Capacitor (v,_) | Inductor (v,_) | CurrentSource (v,_) -> v
        | _ -> failwithf "makeNumberOfBitsField called with invalid component"

    let min,max,step =
        match extractedValue with
        |v when (v>=0.000000001 && v<0.00000001) -> 0.000000001,0.0000000099,0.0000000001
        |v when (v>=0.00000001 && v<0.0000001) -> 0.00000001,0.000000099,0.000000001
        |v when (v>=0.0000001 && v<0.000001) -> 0.0000001,0.00000099,0.00000001
        |v when (v>=0.000001 && v<0.00001) -> 0.000001,0.0000099,0.0000001
        |v when (v>=0.00001 && v<0.0001) -> 0.00001,0.000099,0.000001
        |v when (v>=0.0001 && v<0.001) -> 0.0001,0.00099,0.00001
        |v when (v>=0.001 && v<0.01) -> 0.001,0.0099,0.0001
        |v when (v>=0.01 && v<0.1) -> 0.01,0.099,0.001
        |v when (v>=0.1 && v<1) -> 0.1,0.99,0.01
        |v when (v>=1 && v<10) -> 1,9.9,0.1
        |v when (v>=10 && v<100) -> 10,99,1
        |v when (v>=100 && v<1000) -> 100,999,10
        |v when (v>=1000 && v<10000) -> 1000,9999,100
        |v when (v>=10000 && v<100000) -> 10000,99999,1000
        |v when (v>=100000 && v<1000000) -> 100000,999999,10000
        |_ -> 1,10,0.01
    
    div [] [
        Slider.slider [ 
            Slider.OnChange (getTextEventValue >> onchange)
            Slider.DefaultValue extractedValue
            Slider.Min min
            Slider.Max max
            Slider.Step step
            ]
        div [] [str "Use the slider to change the component's value and see how it affects the circuit"]
    
    ]


let private makeValueField model (comp:Component) text dispatch =
    let sheetDispatch sMsg = dispatch (Sheet sMsg)
    
    let title, value =
        match comp.Type with
        | Resistor (v,s) -> "Resistance value", s
        | Capacitor (v,s) -> "Capacitance value", s
        | Inductor (v,s) -> "Inductance value", s
        | CurrentSource (v,s) -> "Current value", s
        | _ -> failwithf "makeNumberOfBitsField called with invalid component"


    textValueFormField true title value None (
        fun newValue ->
            if textToFloatValue newValue = None
            then
                let props = errorPropsNotification "Invalid number value"
                dispatch <| SetPropertiesNotification props
            else
                model.Sheet.ChangeRLCIValue sheetDispatch (ComponentId comp.Id) (Option.get (textToFloatValue newValue)) newValue
                //SetComponentLabelFromText model comp text' // change the JS component label
                let lastUsedWidth = model.LastUsedDialogWidth 
                dispatch (ReloadSelectedComponent (lastUsedWidth)) // reload the new component
                dispatch <| SetPopupDialogText (Some newValue)
                dispatch ClosePropertiesNotification
    )


let private makeDescription (comp:Component) model dispatch =
    match comp.Type with
    | Resistor _ -> div [] [str "Ideal Resistor"]
    | Capacitor _ -> div [] [str "Ideal Capacitor"]
    | Inductor _ -> div [] [str "Ideal Inductor"]
    | VoltageSource (DC _) -> div [] [str "DC Voltage Source"]
    | VoltageSource (Sine _) -> div [] [str "Sinusoidal Voltage Source"]
    | CurrentSource _ -> div [] [str "DC Current Source"]
    | DiodeL -> div [] [str "Linearized Diode"]
    | Opamp -> div [] [str "Ideal Operational Amplifier"]
    | Ground -> div [] [str "Ground"]
    |_ -> div [] []
        

let private makeExtraInfo model (comp:Component) text dispatch : ReactElement =
    match comp.Type with
    | Resistor _ 
    | Capacitor _ 
    | Inductor _ 
    | CurrentSource _ ->
        div []
            [
                makeValueField model comp text dispatch
                makeSliderField model comp text dispatch
            ]
    | _ -> div [] []


let viewSelectedComponent (model: ModelType.Model) dispatch =

    let checkIfLabelIsUnique chars (symbols: SymbolT.Symbol list)  =
        match List.exists (fun (s:SymbolT.Symbol) -> s.Component.Label = chars) symbols with
        |true -> Error Constants.labelUniqueMess
        |false -> Ok chars

    let allowNoLabel =
        let symbols = model.Sheet.Wire.Symbol.Symbols
        match model.Sheet.SelectedComponents with
        | [cid] ->
            match Map.tryFind cid symbols with
            //| Some {Component ={Type=MergeWires | SplitWire _ | BusSelection _}} -> true
            | _ -> false
        | _ -> false

    let sheetDispatch sMsg = dispatch (Sheet sMsg)

    /// return an OK label text, or an error message
    let formatLabelText (txt: string) compId =
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        let allowedDotPos = -1
        txt.ToUpper()
        |> (fun chars -> 
            let symbols = model.Sheet.Wire.Symbol.Symbols |> Map.toList |> List.filter (fun (i,s) -> i <> compId) |> List.map snd
            let badChars = 
                chars 
                |> Seq.indexed
                |> Seq.filter (fun (i,ch) -> not (System.Char.IsLetterOrDigit ch) && (ch <> '.'  || i <> allowedDotPos))
                |> Seq.map snd
                |> Seq.map string |> String.concat ""
            match String.length chars with 
            | 0 when allowNoLabel -> 
                Ok ""
            | 0 -> 
                Error "Empty label is not allowed for this component"
            | _ when not (System.Char.IsLetter chars[0]) ->
                Error "Labels must start with a character"
            | _ when badChars.Contains "." && allowedDotPos > 0 ->
                Error $"Custom Component labels can only contain a '.' immediately after the name"
            | _ when badChars.Contains "."  ->
                Error $"Labels of normal components can only contain letters and digits, not '.'"
            | _ when badChars <> "" ->
                Error $"Labels can only contain letters and digits, not '{badChars}'"
            | _ -> 
                let currSymbol = model.Sheet.Wire.Symbol.Symbols[compId]
                match currSymbol.Component.Type with
                |IOLabel ->
                    let allSymbolsNotWireLabel = symbols |> List.filter(fun s -> s.Component.Type <> IOLabel)
                    checkIfLabelIsUnique chars allSymbolsNotWireLabel
                |_ ->
                    checkIfLabelIsUnique chars symbols           
            )

    match model.Sheet.SelectedComponents with
    | [ compId ] ->
        let comp = SymbolUpdate.extractComponent model.Sheet.Wire.Symbol compId
        div [Key comp.Id] [
            // let label' = extractLabelBase comp.Label
            // TODO: normalise labels so they only contain allowed chars all uppercase
            let defaultText = 
                match model.PopupDialogData.Text with
                | None -> comp.Label
                | Some text -> text
            let label' = formatLabelText defaultText compId // No formatting atm
            let labelText = match label' with Ok s -> s | Error e -> defaultText
            readOnlyFormField "Description" <| makeDescription comp model dispatch
            makeExtraInfo model comp labelText  dispatch
            let required = 
                match comp.Type with 
                //| SplitWire _ | MergeWires | BusSelection _ -> false 
                | _ -> true
            let isBad = 
                if model.PopupDialogData.BadLabel then 
                    match label' with 
                    | Ok _ -> None
                    | Error msg -> Some msg
                else    None

            //printfn $"{comp.Label}:{label'} - {isBad} - {label'}"
            textFormField 
                required 
                "Component Name" 
                comp.Label 
                isBad 
                (fun text -> // onChange
                    match formatLabelText text compId with
                    | Error errorMess ->
                        dispatch <| SetPopupDialogBadLabel (true)
                        dispatch <| SetPopupDialogText (Some text)
                    | Ok label -> 
                        setComponentLabel model sheetDispatch comp label
                        dispatch <| SetPopupDialogText (Some label)
                        dispatch <| SetPopupDialogBadLabel (false)
                    dispatch (ReloadSelectedComponent model.LastUsedDialogWidth)) // reload the new component
                ( fun () -> // onDeleteAtEndOfBox
                    let sheetDispatch sMsg = dispatch (Sheet sMsg)
                    let dispatchKey = SheetT.KeyPress >> sheetDispatch
                    dispatchKey SheetT.KeyboardMsg.DEL)
        ]    
    | _ -> 
        match model.CurrentProj with
        |Some proj ->
            let sheetName = proj.OpenFileName
            let sheetLdc = proj.LoadedComponents |> List.find (fun ldc -> ldc.Name = sheetName)
            let sheetDescription = sheetLdc.Description
            match sheetDescription with
            |None ->
                div [] [
                    p [] [str "Select a component in the diagram to view or change its properties, for example number of bits." ]    
                    br []
                    Label.label [] [str "Sheet Description"]
                    Button.button
                        [ 
                            Button.Color IsSuccess
                            Button.OnClick (fun _ ->
                                createSheetDescriptionPopup model None sheetName dispatch
                            )
                        ]
                        [str "Add Description"]
                    ]
            |Some descr ->
                div [] [
                    p [] [str "Select a component in the diagram to view or change its properties, for example number of bits." ]    
                    br []
                    Label.label [] [str "Sheet Description"]
                    p [] [str descr]
                    br []
                    Button.button
                        [
                            Button.Color IsSuccess
                            Button.OnClick (fun _ ->
                                createSheetDescriptionPopup model sheetDescription sheetName dispatch
                            )
                        ]
                        [str "Edit Description"]
                    ]
        |None -> null


