module UpdateHelpers

open Elmish

open Fulma
open Fable.React
open Fable.React.Props
open ElectronAPI
open FilesIO
open ModelType
open ModelHelpers
open CommonTypes
open Extractor
open CatalogueView
open PopupView
open FileMenuView
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson
open Helpers
open NumberHelpers
open DiagramStyle

module Constants =
    let memoryUpdateCheckTime = 300.


///Used to filter specific mouse messages based on mouse data.
let matchMouseMsg (msgSelect: DrawHelpers.MouseT -> bool) (msg : Msg) : bool =
    match msg with
    | Sheet sMsg ->
        match sMsg with
        | SheetT.MouseMsg mMsg ->
            msgSelect mMsg
        | _ -> false
    | _ -> false


let shortDSheetMsg msg = Some "Sheet message"

let shortDisplayMsg (msg:Msg) =
    match msg with
    | Sheet sheetMsg -> shortDSheetMsg sheetMsg
    | JSDiagramMsg (InitCanvas _ )-> Some "JSDiagramMsg.InitCanvas"
    | JSDiagramMsg _ -> None
    | KeyboardShortcutMsg _ -> None
    | ChangeRightTab _ -> None
    | ChangeSimSubTab _ -> None
    | SetHighlighted (comps,conns) -> Some $"SetHighlighted: {comps.Length} comps, {conns.Length} conns"
    | SetSelWavesHighlighted x -> Some $"SetSelWavesHighlighted{x.Length}"
    | SetClipboard _ -> Some "SetClipboard"
    | SetCreateComponent _ -> Some "SetCreateComponent"
    | SetProject _ -> Some "SetProject"
    | UpdateProject _ 
    | UpdateModel _ 
    | UpdateProjectWithoutSyncing _ 
    | ShowPopup _ 
    | ShowStaticInfoPopup _ 
    | ClosePopup 
    | SetPopupDialogBadLabel _ 
    | SetPopupDialogText _ 
    | SetPopupDialogText2 _ 
    | SetPopupDialogText3 _ 
    | SetPopupDialogInt _ 
    | SetPopupDialogInt2 _ 
    | SetPopupDialogBadLabel _ 
    | CloseDiagramNotification
    | SetFilesNotification _ 
    | CloseFilesNotification
    | SetPropertiesNotification _ 
    | ClosePropertiesNotification
    | SetTopMenu _ 
    | ReloadSelectedComponent _ 
    | SetDragMode _ 
    // Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth _ 
    | MenuAction _ 
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetIsLoading _
    | CloseApp
    | ExecutePendingMessages _ 
    | DoNothing
    | StartUICmd _
    | FinishUICmd
    | ReadUserData _
    | SetUserData _
    | SetThemeUserData _ -> None
    | ExecCmd _ -> Some "ExecCmd"
    | ExecFuncInMessage _ -> Some "ExecFuncInMessage"
    | ExecFuncAsynch _ -> Some "ExecFuncAsync"
    | ExecCmdAsynch _ -> Some "ExecCmdAsynch"
    | SendSeqMsgAsynch _ -> Some "SendSeqMsgAsynch"




/// If debugTrace is on print out human readable info on message.
/// Be careful not to do this on mouse moves (there are too many).
/// be careful not to try to ptint simulation result arrays (that would crash the renderer!).
/// optimise for very quick return in the case that debugLevel = 0 (production version)
/// optimise for quick return if nothing is printed.
let getMessageTraceString (msg: Msg) =
    let noDisplayMouseOp (mMsg:DrawHelpers.MouseT) = 
        mMsg.Op = DrawHelpers.Drag || mMsg.Op = DrawHelpers.Move
    let noDisplayMessage = function
        | Sheet (SheetT.Msg.Wire(BusWireT.Msg.Symbol(SymbolT.MouseMsg _ | SymbolT.ShowPorts _ ))) -> true
        | _ -> false

    if JSHelpers.debugLevel = 0 ||
       not (Set.contains "update" JSHelpers.debugTraceUI) ||
       matchMouseMsg noDisplayMouseOp msg ||
       noDisplayMessage msg then
        ""
    else 
        match shortDisplayMsg msg with
        | Some shortName -> shortName
        | None ->
            Helpers.sprintInitial 70 $"{msg}"


let traceMessage startOfUpdateTime (msg:Msg) ((model,cmdL): Model*Cmd<Msg>) =
    if JSHelpers.debugLevel > 0 then
        let str = getMessageTraceString msg
        let rootOfMsg = 
            match str.Split [|' ';'('|] with
            | ss when ss.Length > 0 -> ss.[0]
            | _ -> ""
        TimeHelpers.instrumentInterval rootOfMsg startOfUpdateTime |> ignore
        if str <> "" then printfn "**Upd:%s" str
        Cmd.map (fun msg -> printfn ">>Cmd:%s" (getMessageTraceString msg)) |> ignore
    model,cmdL

    