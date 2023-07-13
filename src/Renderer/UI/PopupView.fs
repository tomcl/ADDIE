(*
    PopupView.fs

    This module provides a handy interface to create popups and notifications.
    Popups and notifications appear similar, but are actually quite different:
    - Model.Popup is a function that takes a STRING and produces a ReactElement.
    - Model.Notifications are a functions that take DISPATCH and produce a
      ReactElement.
    This means that at the moment of creation, a popup must already have the
    dispatch function, while the notification does not. This, in turn, means
    that notifications can be created from messages dispatched by JS code.
*)


(*

Popups must be careful in handling internal state because this cannot be updated by
dispatch as would be expected.

viewpopup model ->
model.Popup model.PopupDialogData -> (PopupDialogData contains memory setup data (widths) but not memory component data)

model.Popup <-- 
    unclosablePopup maybeTitle (body memoryEditorData) maybeFoot extraStyle
        (from showMemoryEditorPopup maybeTitle body maybeFoot extraStyle dispatch)

        Here body contains the relevant state and is generated from:

        let openMemoryEditor memory compId model dispatch : unit =
        ....
        let body = makeEditor memory compId model dispatch
        ....
        showMemoryEditorPopup (Some title) body (Some foot) popupExtraStyle dispatch


Although showPopup

*)

module PopupView



//====================================================================//

open Fulma
open Fulma.Extensions.Wikiki
open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Browser.Types
open ElectronAPI

open JSHelpers
open NumberHelpers
open ModelType
open CommonTypes
open EEExtensions
open Sheet.SheetInterface
open System

module Constants =
    let infoSignUnicode = "\U0001F6C8"


//=======//
//HELPERS//
//=======//

let openInBrowser url =
    (fun _ -> Electron.Electron.electron.shell.openExternal url |> ignore)

let extractLabelBase (text:string) : string =
    text.ToUpper()
    |> Seq.takeWhile (fun ch -> ch <> '(')
    |> Seq.filter (fun ch -> System.Char.IsLetterOrDigit ch || ch = '_')
    |> Seq.map (fun ch -> ch.ToString())
    |> String.concat ""

let formatLabelAsBus (width:int) (text:string) =
    let text' = extractLabelBase text
    match width with
    | 1 -> text'
    | _ -> sprintf "%s(%d:%d)" (text'.ToUpper()) (width-1) 0
   
let formatLabelFromType compType (text:string) =
    let text' = extractLabelBase text
    match compType with
    | IO -> text'
    | _ -> text'


let formatLabel (comp:Component) (text:string) =
    formatLabelFromType comp.Type (text:string)

// TODO: removed formatLabel for now
let setComponentLabel model (sheetDispatch) (comp:Component) (text:string) =
    // let label = formatLabel comp text
    let label = text.ToUpper() // TODO

    model.Sheet.ChangeLabel sheetDispatch (ComponentId comp.Id) label
    //model.Diagram.EditComponentLabel comp.Id label



//========//
// Popups //
//========//

let preventDefault (e: Browser.Types.ClipboardEvent) = e.preventDefault()

let getText (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text
let getText2 (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text2
let getText3 (dialogData : PopupDialogData) =
    Option.defaultValue "" dialogData.Text3

let getInt (dialogData : PopupDialogData) =
    Option.defaultValue 1 dialogData.Int

let getInt2 (dialogData : PopupDialogData) : int64 =
    Option.defaultValue 0L dialogData.Int2

/// Unclosable popup.
let unclosablePopup maybeTitle (body:ReactElement) (maybeFoot: ReactElement option) extraStyle =
    fun dispatch  ->
        let propStyle extraStyle  = Props [Style (UserSelect UserSelectOptions.None :: extraStyle)]
        let head =
            match maybeTitle with
            | None -> div [] []
            | Some title -> Modal.Card.head [propStyle []] [ Modal.Card.title [] [ str title ] ]
        let foot =
            match maybeFoot with
            | None -> div [] []
            | Some foot -> Modal.Card.foot [] [ foot ]
        Modal.modal [ Modal.IsActive true ] [
            Modal.background [] []
            Modal.Card.card [Props [Style  extraStyle]] [
                head
                Modal.Card.body [Props [Style [UserSelect UserSelectOptions.None]]] [ body ]
                foot
            ]
        ]

let noDispatch (react: ReactElement) =
    fun (_dispatch: Msg->Unit) -> react

let mapNoDispatch (optReact: ReactElement option) =
    Option.map noDispatch optReact


let private buildPopup title body foot close extraStyle =
    fun (dispatch:Msg->Unit) (dialogData : PopupDialogData) ->
        Modal.modal [ Modal.IsActive true; Modal.CustomClass "modal1"; Modal.Props [Style [ZIndex 20000]]] [
            Modal.background [ Props [ OnClick (close dispatch)]] []
            Modal.Card.card [ Props [
                Style ([
                    OverflowY OverflowOptions.Auto
                    OverflowX OverflowOptions.Visible
                    UserSelect UserSelectOptions.None
                    ] @ extraStyle)
                ] ] [
                Modal.Card.head [] [
                    Modal.Card.title [] [ str title ]
                    Delete.delete [ Delete.OnClick (close dispatch) ] []
                ]
                Modal.Card.body [Props [Style [ OverflowY OverflowOptions.Visible ;OverflowX OverflowOptions.Visible]]] [ body dispatch dialogData ]
                Modal.Card.foot [] [ foot dispatch dialogData ]
            ]
        ]

/// Body and foot are functions that take a string of text and produce a
/// reactElement. The meaning of the input string to those functions is the
/// content of PopupDialogText (i.e. in a dialog popup, the string is the
/// current value of the input box.).
let dynamicClosablePopup title (body:PopupDialogData -> ReactElement) (foot: PopupDialogData -> ReactElement) (extraStyle: CSSProp list) (dispatch: Msg->Unit) =
    buildPopup title (fun _ -> body) (fun _ -> foot) (fun dispatch _ -> dispatch ClosePopup) extraStyle
    |> ShowPopup |> dispatch

/// As dynamicClosablePopup but accept functions of dispatch and return the popup function
let private dynamicClosablePopupFunc title body foot extraStyle =
        buildPopup title body foot (fun dispatch _ -> dispatch ClosePopup) extraStyle

/// Popup to track progress of some long operation. Progress is captured via two dialog integers, current and max number.
/// Typically the number is number of steps.
/// The popup display is controlled by model.PopupDialog integers. Progress model updates must change these.
let dynamicProgressPopupFunc title (cancel: (Msg -> Unit) -> Unit) =
    let body (dispatch:Msg->Unit) (dialog:PopupDialogData) =
        let n = Option.defaultValue 0 dialog.Int        
        Progress.progress
            [   Progress.Color IsSuccess
                Progress.Value n
                Progress.Max (int (Option.defaultValue 100L (dialog.Int2))) ]
            [ str $"{n}"]

    let foot (dispatch:Msg->Unit) (dialog:PopupDialogData) =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> 
                            cancel dispatch
                            dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
            ]
        ]
        
    buildPopup title body foot (fun dispatch _ -> dispatch ClosePopup) []

/// Create a popup and add it to the page. Body and foot are static content.
/// Can be closed by the ClosePopup message.
let closablePopup title body foot extraStyle dispatch =
    dynamicClosablePopup title (fun _ -> body) (fun _ -> foot) extraStyle dispatch

/// As closablePopup but accept functions and return the popup function
/// Can be closed by the ClosePopup message.
let closablePopupFunc title (body:(Msg->Unit)->ReactElement) (foot:(Msg->Unit)->ReactElement) extraStyle  =
        dynamicClosablePopupFunc title (fun dispatch _ -> body dispatch) (fun dispatch _ -> foot dispatch) extraStyle

/// Create the body of a dialog Popup with only text.
let dialogPopupBodyOnlyText before placeholder dispatch =
    fun (dialogData : PopupDialogData) ->
        let goodLabel =
                getText dialogData
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> true | Some ch -> false | None -> true
        div [] [
            before dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]            

        ]

/// Create the body of a dialog Popup with only text.
let dialogPopupBodyNumericalText before placeholder dispatch =
    fun (dialogData : PopupDialogData) ->
        let goodLabel =
            match textToFloatValue (getText dialogData) with
            |Some _ -> true
            |None -> false
                
        div [] [
            before dialogData
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Invalid number format"]            

        ]

/// Create the body of a dialog Popup with only text for sheet description (can have an initial value + allow empty).
let dialogPopupBodyOnlyTextWithDefaultValue before placeholder currDescr dispatch =
    fun (dialogData : PopupDialogData) ->
        let defaultValue = Option.defaultValue "" currDescr
        div [] [
            before dialogData
            Input.text [
                Input.DefaultValue defaultValue
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]



/// Create the body of a dialog Popup with only an int.
let dialogPopupBodyOnlyInt beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]; AutoFocus true]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]


/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyTextAndInt beforeText placeholder beforeInt intDefault dispatch =
    
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        let goodLabel =
                getText dialogData
                |> Seq.toList
                |> List.tryHead
                |> function | Some ch when  System.Char.IsLetter ch -> true | Some ch -> false | None -> true
        div [] [
            beforeText dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Name must start with a letter"]            
            br []
            br []
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
        ]

// Pulse is not used currently
// uncomment from select options below to include it again
let dialogPopupVS dispatch =
    fun (dialogData : PopupDialogData) ->
        let goodLabel =
            match textToFloatValue (getText dialogData) with
            |Some _ -> true
            |None -> false
        
        let placeholderV = "0.0V"
        let placeholderF = "0Hz"
        
        let before1 = 
            match dialogData.VSType with
            |Some "Sine" -> "Amplitude (V)" |> str
            |Some "Pulse" -> "V1 (V)" |> str
            | _ -> "DC Voltage value (V)" |> str
        let before2 = 
            match dialogData.VSType with
            |Some "Sine" -> "DC Offset (V)" |> str
            |Some "Pulse" -> "V2 (V)" |> str
            | _ -> "DC Voltage value (V)" |> str
        let before3 = 
            match dialogData.VSType with
            |Some "Sine" 
            |Some "Pulse" -> "Frequency (F)" |> str
            | _ -> "DC Voltage value (V)" |> str


        
        let secondInput = 
            match dialogData.VSType with
            |Some "Sine" |Some "Pulse" ->
                [
                    br []
                    before2
                    Input.text [
                        Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                        Input.Placeholder placeholderV
                        Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText2 >> dispatch)
                    ]
                    br []]
            |_ -> []

        let thirdInput = 
            match dialogData.VSType with
            |Some "Sine" | Some "Pulse" ->
                [
                br []
                before3
                Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholderF
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText3 >> dispatch)
                ]]
            |_ -> []
        
        div [] [
            Label.label [] [ str "Voltage Source Type" ]
            Label.label [ ]
                [Select.select []
                [ select [(OnChange(fun option -> 
                    printfn "Value is: %s" option.Value
                    SetPopupDialogVSType (Some option.Value) |> dispatch))]
                    ([option [Value "DC";Selected true] [str ("DC")]] @ [option [Value "Sine"] [str "Sine"] ])// @ [option [Value "Pulse"] [str "Pulse"] ])
                    ]
                ]
            before1
            Input.text [
                Input.Props [AutoFocus true; SpellCheck false]
                Input.Placeholder placeholderV
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
            span [Style [FontStyle "Italic"; Color "Red"]; Hidden goodLabel] [str "Invalid number format"]            
            div [] secondInput
            div [] thirdInput
        ]


/// Create the body of a dialog Popup with both text and int.
let dialogPopupBodyIntAndText beforeText placeholder beforeInt intDefault dispatch =
    intDefault |> Some |> SetPopupDialogInt |> dispatch
    fun (dialogData : PopupDialogData) ->
        div [] [
            beforeInt dialogData
            br []
            Input.number [
                Input.Props [OnPaste preventDefault; Style [Width "60px"]]
                Input.DefaultValue <| sprintf "%d" intDefault
                Input.OnChange (getIntEventValue >> Some >> SetPopupDialogInt >> dispatch)
            ]
            br []
            br []
            beforeText dialogData
            Input.text [
                Input.Props [OnPaste preventDefault; AutoFocus true; SpellCheck false]
                Input.Placeholder placeholder
                Input.OnChange (getTextEventValue >> Some >> SetPopupDialogText >> dispatch)
            ]
        ]


/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
let dialogPopup title body buttonText buttonAction isDisabled extraStyle dispatch =
    let foot =
        fun (dialogData : PopupDialogData) ->
            Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsLight
                            Button.OnClick (fun _ -> 
                                dispatch ClosePopup
                                dispatch FinishUICmd) //In case user presses cancel on 'rename sheet' popup
                        ] [ str "Cancel" ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Disabled (isDisabled dialogData)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> buttonAction dialogData)
                        ] [ str buttonText ]
                    ]
                ]
            ]
    dynamicClosablePopup title body foot extraStyle dispatch

/// Popup with an input textbox and two buttons.
/// The text is reflected in Model.PopupDialogText.
let dialogVerilogPopup title body saveUpdateText noErrors showingExtraInfo saveButtonAction moreInfoButton isDisabled extraStyle dispatch =
    let foot =
        fun (dialogData : PopupDialogData) ->
            let compileButtonText = 
                if noErrors then "Compiled" 
                elif showingExtraInfo then "Hide Info"
                else "More Info" 
            let compileButtonColor = if noErrors then IsInfo else IsDanger
            Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
                Level.left [] []
                Level.right [] [
                    Level.item [] [
                        Button.button [
                            Button.Color IsLight
                            Button.OnClick (fun _ -> 
                                dispatch ClosePopup
                                dispatch FinishUICmd) //In case user presses cancel on 'rename sheet' popup
                        ] [ str "Cancel" ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Disabled (noErrors)
                            Button.Color compileButtonColor
                            Button.OnClick (fun _ -> moreInfoButton dialogData)
                        ] [ str compileButtonText ]
                    ]
                    Level.item [] [
                        Button.button [
                            Button.Disabled (isDisabled dialogData)
                            Button.Color IsPrimary
                            Button.OnClick (fun _ -> saveButtonAction dialogData)
                        ] [ str saveUpdateText ]
                    ]
                ]
            ]
    dynamicClosablePopup title body foot extraStyle dispatch

/// A static confirmation popup.
let confirmationPopup title body buttonText buttonAction dispatch =
    let foot =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick buttonAction
                    ] [ str buttonText ]
                ]
            ]
        ]
    closablePopup title body foot [] dispatch

/// A static choice dialog popup returning the popup function
let choicePopupFunc 
        title 
        (body:(Msg->Unit)->ReactElement) 
        buttonTrueText 
        buttonFalseText 
        (buttonAction: bool -> (Msg->Unit) -> Browser.Types.MouseEvent -> Unit) =
    let foot dispatch =
        Level.level [ Level.Level.Props [ Style [ Width "100%" ] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (buttonAction false dispatch)
                    ] [ str buttonFalseText ]
                ]
                Level.item [] [
                    Button.button [
                        Button.Color IsPrimary
                        Button.OnClick (buttonAction true dispatch)
                    ] [ str buttonTrueText ]
                ]
            ]
        ]
    closablePopupFunc title body foot []

/// A static choice dialog popup.
let choicePopup title (body:ReactElement) buttonTrueText buttonFalseText (buttonAction: bool -> Browser.Types.MouseEvent -> Unit) dispatch =
    let popup = choicePopupFunc title (fun _ -> body) buttonTrueText buttonFalseText (fun bool dispatch-> buttonAction bool)
    dispatch <| ShowPopup popup


/// Popup to implement spinner for long operations
let viewSpinnerPopup (spinPayload:SpinPayload) (model: Model) (dispatch: (Msg -> Unit)) =
    dispatch <| UpdateModel spinPayload.Payload
    let body (dispatch: Msg->Unit) (dialog: PopupDialogData) =
        Progress.progress
            [   Progress.Color IsSuccess
                Progress.Value (spinPayload.Total - spinPayload.ToDo)
                Progress.Max (spinPayload.Total)
            ]
            [ str $"{spinPayload.Total - spinPayload.ToDo}"]

    let foot (dispatch:Msg->Unit) (dialog:PopupDialogData) =
        Level.level [ Level.Level.Props [ Style [ Width "100%"] ] ] [
            Level.left [] []
            Level.right [] [
                Level.item [] [
                    Button.button [
                        Button.Color IsLight
                        Button.OnClick (fun _ -> 
                            dispatch ClosePopup)
                    ] [ str "Cancel" ]
                ]
            ]
        ]
        
    buildPopup spinPayload.Name body foot (fun dispatch _ -> dispatch ClosePopup) [] dispatch model.PopupDialogData

/// Display popup, if any is present.
/// A progress popup, if present, overrides any display popup.
/// A spinner popup, if present, overrides all other popups
let viewPopup model dispatch =
    match model.PopupViewFunc, model.SpinnerPayload with
    | None, None -> 
        div [] []
    | _, Some payload ->
        viewSpinnerPopup payload model dispatch
    | Some popup, _ -> popup dispatch model.PopupDialogData
    

let makeH h =
    Text.span [ Modifiers [
        Modifier.TextSize (Screen.Desktop, TextSize.Is6)
        Modifier.TextWeight TextWeight.Bold
    ] ] [str h; br []]
let styledSpan styles txt = span [Style styles] [str <| txt]
let bSpan txt = styledSpan [FontWeight "bold"] txt
let iSpan txt = styledSpan [FontStyle "italic"] txt
let tSpan txt = span [] [str txt]


let makeInfoPopupButton (title: string) (info: ReactElement) dispatch =

    let foot _ = div [] []
    let popup dispatch = dynamicClosablePopup title (fun _ -> info) foot [Width 1000] dispatch
    // button driving a popup with a page of info
    Button.button
        [
            Button.OnClick (fun _ -> popup dispatch)
            Button.Size IsSmall
            Button.IsRounded
            Button.Color IColor.IsInfo
            Button.Props [Style [
                Height "32px"
                FontSize "24px"; 
                MarginLeft "10px"; 
                MarginRight "10px"; 
                MarginTop "3px";
                MarginBottom "0px"
                Padding "5px"; 
                PaddingTop "5px"; 
                PaddingBottom "8px"]]
        ]
        [str Constants.infoSignUnicode]

let viewInfoPopup dispatch =

    let title = "ADDIE: Analog Design & Debugging Integrated Environment"

    let about = div [] [
        makeH "Version"
        str Version.VersionString
        br []; br []
        makeH "Acknowledgments"
        str "ADDIE was created by Archontis Pantelopoulos (EIE 4th year) as his MEng final year project, \
             using the pre-existing digital electronics equivalent: Addie."
        br []; br [] 
        makeH "Technology"
        Text.span [] [
            str "ADDIE is written in "
            a [OnClick <| openInBrowser "https://fsharp.org/"] [str "F#"] 
            str " compiled to Javascript by "
            a [OnClick <| openInBrowser "https://fable.io/"] [str "FABLE"]
            str " and running under the "
            a [OnClick <| openInBrowser "https://www.electronjs.org/"] [str "Electron"]
            str " framework" 
            ]    
        ]

    let intro = div [] [
        str "Addie currently supports only linear components (resistors, capacitors, inductors, opamps), \ 
        DC Current Sources, DC and Sinusoidal Voltage Sources, and linearized (0.7 Voltage Drop) diodes." 
        br []; br []
        str "Addie supports three types of simulation which can be found under the 'Simulations' tab. These are: (i) DC \
        Operating Point Analysis, (ii) Frequency Response, and (iii) Time Domain Simulations." 
        br []  ; br [];  
        button 
            [OnClick <| openInBrowser "https://github.com/apantelopoulos/ADDIE"] 
            [ str "See the Addie Github Repo for more information"]
        br [] ; br [] ]

    let bugReport = 
        let oTextList txtL = Content.content [] [Content.Ol.ol [] (List.map (fun txt -> li [] [str txt]) txtL)]
        div [] [
            str
                "If you think ADDIE is not working it is very helpful if you can give us details: we usually answer \
                and fix bugs, if they exist, very quickly. Before you contact us, look at the list below and answer as much \
                as possible to make your Bug Report (sometimes it is not all possible, send what you can)."
            oTextList 
                [
                    "Which version of Addie (Info tab, About Addie)"
                    "Which platform (Windows, Macos)"    
                    "What did you do that led to unexpected behaviour?"   
                    "What result did you expect?"   
                    "What result did you get?"   
                    "What project files caused this, the top-level sheet? Enclose project as zipped file \
                    deleting the maybe large backup directory when you zip."   
                    "If you can reproduce the bug yourself, try opening dev tools (Ctrl-Shift-I). You can do this after the bug happens. 2/3 \
                    of problems result in error messages displayed there. Screenshot the error and its backtrace and send it."   
                    "What precise actions (if you know them) led to the bug after loading this project"
                ]
            ]
    let keyOf2 s1 s2 = span [] [bSpan s1; tSpan " + "; bSpan s2]
    let keyOf3 s1 s2 s3 = span [] [bSpan s1; tSpan " + "; bSpan s2 ; tSpan " + "; bSpan s3]
    let rule = hr [Style [MarginTop "0.5em"; MarginBottom "0.5em"]]
    let keys = div [] [
        makeH "Keyboard & mouse gesture shortcuts - also available on top menus"
        span [Style [FontStyle "Italic"]] [str "On Mac use Cmd instead of Ctrl."]
        ul [] [
            li [] [rule; tSpan "Save: "; keyOf2 "Ctrl" "S"; rule]
            li [] [tSpan "Select all: " ; keyOf2  "Ctrl" "A"]
            li [] [tSpan "Copy selected diagram items: " ; keyOf2  "Ctrl" "C"]
            li [] [tSpan "Paste diagram items: " ; keyOf2  "Ctrl" "V"; rule]
            li [] [tSpan "Undo last diagram action: " ; keyOf2  "Ctrl" "Z"]
            li [] [tSpan "Redo last diagram action: " ; keyOf2  "Ctrl" "Y"; rule]
            li [] [tSpan "Zoom application in: " ; keyOf3  "Ctrl" "Shift" "="]
            li [] [tSpan "Zoom application out: " ; keyOf3  "Ctrl" "Shift" "-"; rule]
            li [] [tSpan "Zoom canvas in/out: " ; keyOf2  "Ctrl" "MouseWheel"]
            li [] [tSpan "Zoom canvas in: " ; keyOf2  "Shift" "="]
            li [] [tSpan "Zoom canvas out: " ; keyOf2  "Shift" "-"; rule]
            li [] [tSpan "Zoom circuit to fit screen: " ; keyOf2  "Ctrl" "W"]
            li [] [tSpan "Scroll (mouse): " ; keyOf2 "Shift" "Left-Click"; bSpan " on canvas and drag"]
            li [] [tSpan "Scroll (touch-pad): " ; bSpan "Two-finger scrolling on touchpad"]
            li [] [tSpan "Scroll (touch-screen): " ; bSpan "One-finger drag on screen"]
        ] ]
    let body (dialogData:PopupDialogData) =
        
        let tab = dialogData.Int

        div [] [
            Tabs.tabs 
                [ Tabs.IsFullWidth
                  Tabs.IsBoxed ]
                [ Tabs.tab [ Tabs.Tab.IsActive (tab = Some 0) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 0)) ]
                    [ str "About Addie" ] ]
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 2) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 2)) ]
                    [ str "Introduction" ] ]  
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 1) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 1)) ]
                    [ str "Keyboard Shortcuts" ] ]
                  Tabs.tab [ Tabs.Tab.IsActive (tab = Some 3) ]
                    [ a [ OnClick (fun _ -> dispatch <| SetPopupDialogInt (Some 3)) ]
                    [ str "Bug Reports" ] ] ]

            match tab with
            | Some 0 -> about
            | Some 1 -> keys
            | Some 2 -> intro
            | Some 3 -> bugReport
            | _ -> dispatch <| SetPopupDialogInt (Some 0)
        ]

    let foot _ = div [] []
    dynamicClosablePopup title body foot [Width 800] dispatch

let viewWaveSelectConfirmationPopup numWaves action dispatch =
    let makeH h =
        Text.span [ Modifiers [
            Modifier.TextSize (Screen.Desktop, TextSize.Is6)
            Modifier.TextWeight TextWeight.Bold
        ] ] [str h; br []]
    let styledSpan styles txt = span [Style styles] [str <| txt]
    let bSpan txt = styledSpan [FontWeight "bold"] txt
    let iSpan txt = styledSpan [FontStyle "italic"] txt
    let tSpan txt = span [] [str txt]
    
    let title = "Warning"
    
    let warning = 
        div [] [
            str $"You have selected {numWaves} waveforms. "
            str "Consider reducing this number to less than 20. Too many waveforms selected in the viewer may impact viewer reponsiveness. \
                 Best practice is to select only the waveforms you need to view."
        ]  
       
    let body (dialogData:PopupDialogData) =
        warning
    let foot _ = div [] []
    choicePopup title warning "Select waveforms" "Change selection"  action dispatch


let makePopupButton (title: string) (menu: PopupDialogData -> ReactElement) (buttonLegend: string) dispatch =

    let foot _ = div [] []
    let popup dispatch = 
        dynamicClosablePopup title (fun dialog -> menu dialog) foot [Width 600] dispatch
    // button driving a popup with a page of info
    Button.button
        [
            Button.OnClick (fun _ -> popup dispatch)
            Button.Color IsPrimary
        ]
        [str buttonLegend]





