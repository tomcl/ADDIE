module ComponentCreation

open Fable.React
open Optics
open Optic

open ModelType
open ModelHelpers
open CommonTypes
open NumberHelpers
open PopupView
open DrawModelType



let createComponent compType label model dispatch =
    Sheet (SheetT.InitialiseCreateComponent (tryGetLoadedComponents model, compType, label)) |> dispatch

// Anything requiring a standard label should be checked and updated with the correct number suffix in Symbol/Sheet, 
// so give the label ""
let createCompStdLabel comp model dispatch =
    createComponent comp "" model dispatch


let createRCLIPopup (model:Model) (compType:ComponentType) dispatch =
    let compname,before, placeholder = 
        match compType with 
        |Resistor _ -> "resistor", "Resistance Value ("+omegaString+")", "0.0 "+ omegaString
        |Capacitor _ -> "capacitor", "Capacitance Value (F)", "0.0 F"
        |Inductor _ -> "inductor", "Inductance value (H)" , "0.0 H"
        |CurrentSource _ -> "current source", "Current value (A)", "0.0 A"
        |_ -> failwithf ""
    let title = sprintf "Add a "+compname
    let beforeText =
        fun _ -> before |> str
    let body = dialogPopupBodyNumericalText beforeText placeholder dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->
            let inputValue = getText dialogData
            let value = Option.get (textToFloatValue inputValue)
            let newComp =
                match compType with
                |Resistor _ -> Resistor (value,inputValue)
                |Capacitor _ -> Capacitor (value,inputValue)
                |Inductor _ -> Inductor (value,inputValue)
                |CurrentSource _ -> CurrentSource (value,inputValue)
                |_ -> failwithf ""
            createCompStdLabel newComp model dispatch
            dispatch ClosePopup
    let isDisabled =
        fun (dialogData : PopupDialogData) -> 
            match textToFloatValue (getText dialogData) with
            |Some f -> false
            |None -> true
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch


let createVSPopup (model:Model) (compType:ComponentType) dispatch =
    
    let title = sprintf "Add a voltage source"
    let body = dialogPopupVS dispatch
    let buttonText = "Add"
    let buttonAction =
        fun (dialogData : PopupDialogData) ->

            
            let inputValue1 = getText dialogData
            let value1 = Option.get (textToFloatValue inputValue1)
            let inputValue2 = getText2 dialogData
            let value2 = Option.get (textToFloatValue inputValue2)
            let inputValue3 = getText3 dialogData
            let value3 = Option.get (textToFloatValue inputValue3)
            
            let newComp =
                match dialogData.VSType with
                |Some "Sine" -> VoltageSource (Sine (value1,value2,value3,0.))
                |Some "Pulse" -> VoltageSource (Pulse (value1,value2,value3))
                |_-> VoltageSource (DC value1)
                
            createCompStdLabel newComp model dispatch
            dispatch ClosePopup
            dispatch (SetPopupDialogVSType (Some "DC"))
    let isDisabled =
        fun (dialogData : PopupDialogData) -> 
            match textToFloatValue (getText dialogData) with
            |Some f -> false
            |None -> true
    dialogPopup title body buttonText buttonAction isDisabled [] dispatch
