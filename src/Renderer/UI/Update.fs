module Update

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
open CanvasStateAnalyser
open UpdateHelpers
open Optics
open Optic

open Operators

//---------------------------------------------------------------------------------------------//
//---------------------------------------------------------------------------------------------//
//---------------------------------- Update Model ---------------------------------------------//
//---------------------------------------------------------------------------------------------//






/// Read persistent user data from file in userAppDir.
/// Store in Model UserData.
let private readUserData (userAppDir: string) (model: Model) : Model * Cmd<Msg> =
    let addAppDirToUserData model = 
        {model with UserData = {model.UserData with UserAppDir = Some userAppDir}}

    let modelOpt =
        try
            let jsonRes = tryReadFileSync <| pathJoin [|userAppDir;"IssieSettings.json"|]
            jsonRes
            |> Result.bind (fun json -> Json.tryParseAs<UserData> json)
            |> Result.bind (fun (data: UserData) -> Ok {model with UserData = data})
            |> (function | Ok model -> model | Error _ -> printfn "Error reading user data" ; model)
            |> addAppDirToUserData 
            |> userDataToDrawBlockModel
            |> Some
        with
        | e -> None
    match modelOpt with
    | Some model -> model, Cmd.none
    | None -> addAppDirToUserData model, Cmd.none

let private writeUserData (model:Model) =
    model.UserData.UserAppDir
    |> Option.map (fun userAppDir ->
        try
            let data = drawBlockModelToUserData model model.UserData
            Json.serialize<UserData> data |> Ok
        with
        | e -> Error "Can't write settings on this PC because userAppDir does not exist"
        |> Result.bind (fun json -> writeFile (pathJoin [|userAppDir;"IssieSettings.json"|]) json)
        |> Result.mapError (fun mess -> $"Write error on directory {userAppDir}: %s{mess}")
        |> function | Error mess -> printfn "%s" mess | _ -> ())
    |> ignore


/// handle Menu actions that may need Model data
let getMenuView (act: MenuCommand) (model: Model) (dispatch: Msg -> Unit) =
    match act with
    | MenuSaveFile -> 
        FileMenuView.saveOpenFileActionWithModelUpdate model dispatch |> ignore
        SetHasUnsavedChanges false
        |> JSDiagramMsg |> dispatch
    | MenuNewFile -> 
        FileMenuView.addFileToProject model dispatch
    | MenuExit ->
        FileMenuView.doActionWithSaveFileDialog "Exit ISSIE" CloseApp model dispatch ()
    
            
    | _ -> ()
    model

/// get timestamp of current loaded component.
/// is this ever used? No.
let getCurrentTimeStamp model =
    match model.CurrentProj with
    | None -> System.DateTime.MinValue
    | Some p ->
        p.LoadedComponents
        |> List.tryFind (fun lc -> lc.Name = p.OpenFileName)
        |> function | Some lc -> lc.TimeStamp
                    | None -> failwithf "Project inconsistency: can't find component %s in %A"
                                p.OpenFileName ( p.LoadedComponents |> List.map (fun lc -> lc.Name))

/// Replace timestamp of current loaded component in model project by current time
/// Used in update function
let updateTimeStamp model =
    let setTimeStamp (lc:LoadedComponent) = {lc with TimeStamp = System.DateTime.Now}
    match model.CurrentProj with
    | None -> model
    | Some p ->
        p.LoadedComponents
        |> List.map (fun lc -> if lc.Name = p.OpenFileName then setTimeStamp lc else lc)
        |> fun lcs -> { model with CurrentProj=Some {p with LoadedComponents = lcs}}

//Finds if the current canvas is different from the saved canvas
// waits 50ms from last check

let findChange (model : Model) : bool = 
    let last = model.LastChangeCheckTime // NB no check to reduce total findChange time implemented yet - TODO if needed
    let start = TimeHelpers.getTimeMs()

    match model.CurrentProj with
    | None -> false
    | Some prj ->
        //For better efficiency just check if the save button
        let savedComponent = 
            prj.LoadedComponents
            |> List.find (fun lc -> lc.Name = prj.OpenFileName)
        let canv = savedComponent.CanvasState
        let canv' = model.Sheet.GetCanvasState ()
        (canv <> canv') && not (compareCanvas 100. canv canv')
        |> TimeHelpers.instrumentInterval "findChange" start

/// Needed so that constant properties selection will work
/// Maybe good idea for other things too?
let resetDialogIfSelectionHasChanged newModel oldModel =
    let newSelected = newModel.Sheet.SelectedComponents
    if newSelected.Length = 1 && newSelected <> oldModel.Sheet.SelectedComponents then
        newModel
        |> map popupDialogData_ (
            set text_ None >>
            set int_ None
        )


    else newModel
   
let exitApp (model:Model) =
    // send message to main process to initiate window close and app shutdown
    writeUserData model
    renderer.ipcRenderer.send("exit-the-app",[||])

///Tests physical equality on two objects
///Used because Msg type does not support structural equality
let isSameMsg = LanguagePrimitives.PhysicalEquality 

let doBatchOfMsgsAsynch (msgs: seq<Msg>) =
    msgs
    |> Seq.map Elmish.Cmd.ofMsg 
    |> Elmish.Cmd.batch
    |> ExecCmdAsynch
    |> Elmish.Cmd.ofMsg

///Returns None if no mouse drag message found, returns Some (lastMouseMsg, msgQueueWithoutMouseMsgs) if a drag message was found
let getLastMouseMsg msgQueue =
    msgQueue
    |> List.filter (matchMouseMsg (fun mMsg -> mMsg.Op = DrawHelpers.Drag))
    |> function
    | [] -> None
    | lst -> Some lst.Head //First item in the list was the last to be added (most recent)

let sheetMsg sMsg model = 
    let sModel, sCmd = SheetUpdate.update sMsg model.Sheet
    let newModel = { model with Sheet = sModel} 
    {newModel with SavedSheetIsOutOfDate = findChange newModel}, Cmd.map Sheet sCmd

//----------------------------------------------------------------------------------------------------------------//
//-----------------------------------------------UPDATE-----------------------------------------------------------//
//----------------------------------------------------------------------------------------------------------------//

/// Main MVU model update function
let update (msg : Msg) oldModel =
    let startOfUpdateTime = TimeHelpers.getTimeMs()
    

    //Add the message to the pending queue if it is a mouse drag message
    let model =
        if matchMouseMsg (fun mMsg -> mMsg.Op = DrawHelpers.Drag) msg then
            {oldModel with Pending = msg :: oldModel.Pending}
        else
            oldModel
    
    //Check if the current message is stored as pending, if so execute all pending messages currently in the queue
    let testMsg, cmd =
        List.tryFind (fun x -> isSameMsg x msg) model.Pending
        |> function
        | Some _ ->
            //Add any message recieved to the pending message queue
            DoNothing, Cmd.ofMsg (ExecutePendingMessages (List.length model.Pending))
        | None ->
            msg, Cmd.none
    // main message dispatch match 
    match testMsg with
    | StartUICmd uiCmd ->
        //printfn $"starting UI command '{uiCmd}"
        match model.UIState with
        | None -> //if nothing is currently being processed, allow the ui command operation to take place
            match uiCmd with
            | CloseProject ->
                {model with CurrentProj = None; UIState = Some uiCmd}, Cmd.none
            | _ -> 
                {model with UIState = Some uiCmd}, Cmd.ofMsg (Sheet (SheetT.SetSpinner true))
        | _ -> model, Cmd.none //otherwise discard the message
    | FinishUICmd _->
        //printfn $"ending UI command '{model.UIState}"
        let popup = CustomCompPorts.optCurrentSheetDependentsPopup model
        {model with UIState = None; PopupViewFunc = popup}, Cmd.ofMsg (Sheet (SheetT.SetSpinner false))

    (*| ShowExitDialog ->
        match model.CurrentProj with
        | Some p when model.SavedSheetIsOutOfDate ->
            {model with ExitDialog = true}, Cmd.none
        | _ -> // exit immediately since nothing to save
            exitApp()
            model, Cmd.none*)
    | CloseApp ->
        exitApp model
        model, Cmd.none
    (*| SetExitDialog status ->
        {model with ExitDialog = status}, Cmd.none*)
    | Sheet sMsg ->
        match sMsg, model.PopupViewFunc with
        | SheetT.ToggleNet canvas, _ ->
            model, Cmd.none
        | SheetT.KeyPress _, Some _ -> 
            // do not allow keys to affect Sheet when popup is on.
            model, Cmd.none
        | _ -> sheetMsg sMsg model
    // special mesages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | SetDragMode mode -> {model with DividerDragMode= mode}, Cmd.none
    | SetViewerWidth w ->
        model, Cmd.none
    | ReloadSelectedComponent width ->
        {model with LastUsedDialogWidth=width}, Cmd.none
    | UpdateModel( updateFn: Model -> Model) ->
        updateFn model, Cmd.none
    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        firstTip <- true
        { model with RightPaneTabVisible = newTab }, 
        match newTab with 
        | Properties -> Cmd.batch <| editCmds
        | Catalogue -> Cmd.batch  <| editCmds
        | Simulation -> Cmd.batch <| editCmds
        //| TruthTable -> Cmd.batch <| editCmds
    | ChangeSimSubTab subTab ->
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        { model with SimSubTabVisible = subTab},
        match subTab with
        | DCsim -> Cmd.batch <| editCmds
        | ACsim-> Cmd.batch <| editCmds
        | TimeSim -> Cmd.batch <| editCmds
    | SetHighlighted (componentIds, connectionIds) ->
        let sModel, sCmd = SheetUpdate.update (SheetT.ColourSelection (componentIds, connectionIds, HighLightColor.Red)) model.Sheet
        {model with Sheet = sModel}, Cmd.map Sheet sCmd
    | SetSelWavesHighlighted connIds ->
        let wModel, wCmd = SheetUpdate.update (SheetT.ColourSelection ([], Array.toList connIds, HighLightColor.Blue)) model.Sheet
        {model with Sheet = wModel}, Cmd.map Sheet wCmd
    | SetClipboard components -> { model with Clipboard = components }, Cmd.none
    | SetCreateComponent pos -> { model with LastCreatedComponent = Some pos }, Cmd.none
    | SetProject project ->
        model
        |> set currentProj_ (Some project) 
        |> set (popupDialogData_ >-> projectPath_) project.ProjectPath, Cmd.none
    | UpdateProject update ->
        CustomCompPorts.updateProjectFiles true update model, Cmd.none
    | UpdateProjectWithoutSyncing update -> 
        CustomCompPorts.updateProjectFiles false update model,Cmd.none
    | ShowPopup popup -> { model with PopupViewFunc = Some popup }, Cmd.none
    | ShowStaticInfoPopup(title, body, dispatch) ->
        let foot = div [] []
        PopupView.closablePopup title body foot [Width 800] dispatch
        model, Cmd.none
    | ClosePopup ->
        { model with
            PopupViewFunc = None;
            PopupDialogData =
                    { model.PopupDialogData with
                        Text = None;
                        Int = None;
                        Int2 = None;
                    }}, Cmd.none
    | SetPopupDialogText text ->
        set (popupDialogData_ >-> text_) text model, Cmd.none
    | SetPopupDialogText2 text ->
        set (popupDialogData_ >-> text2_) text model, Cmd.none
    | SetPopupDialogText3 text ->
        set (popupDialogData_ >-> text3_) text model, Cmd.none
    | SetPopupDialogVSType tp ->
        set (popupDialogData_ >-> vsType_) tp model, Cmd.none
    | SetSimulationACSource tp ->
        set (simulationData_ >-> acSource_) tp model, Cmd.none
    | SetSimulationACOut tp ->
        set (simulationData_ >-> acOutput_) tp model, Cmd.none
    | SetSimulationACInDB ->
        set (simulationData_ >-> acMag_) (not model.SimulationData.ACMagInDB) model, Cmd.none
    | SetSimulationACInHz ->
        set (simulationData_ >-> acFreq_) (not model.SimulationData.ACFreqInHz) model, Cmd.none
    | SetSimulationTimeSource tp ->
        set (simulationData_ >-> timeSource_) tp model, Cmd.none
    | SetSimulationTimeOut tp ->
        set (simulationData_ >-> timeOutput_) tp model, Cmd.none
    | SetPopupDialogBadLabel isBad ->
        set (popupDialogData_ >-> badLabel_) isBad model, Cmd.none
    | SetPopupDialogInt int ->
        set (popupDialogData_ >-> int_) int model, Cmd.none
    | SetPopupDialogInt2 int ->
        set (popupDialogData_ >-> int2_) int model, Cmd.none
    | CloseDiagramNotification ->
        { model with Notifications = {model.Notifications with FromDiagram = None} }, Cmd.none
    | SetFilesNotification n ->
        { model with Notifications =
                        { model.Notifications with FromFiles = Some n} }, Cmd.none
    | CloseFilesNotification ->
        { model with Notifications = {model.Notifications with FromFiles = None} }, Cmd.none
    | SetPropertiesNotification n ->
        { model with Notifications =
                        { model.Notifications with FromProperties = Some n} }, Cmd.none
    | ClosePropertiesNotification ->
        { model with Notifications = { model.Notifications with FromProperties = None} }, Cmd.none
    | SetTopMenu t ->
        { model with TopMenuOpenState = t}, Cmd.none
    | ExecFuncInMessage (f,dispatch)->
        (f model dispatch; model), Cmd.none
    | ExecCmd cmd ->
        model, cmd
    | ExecFuncAsynch func ->
             let cmd' = 
                Elmish.Cmd.OfAsyncImmediate.result (async { 
                //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
                //this number only seems to affect the wavesim spinner cursor, it does not help with open project/change sheet spinner cursor
                    do! (Async.Sleep 100) 
                    if Set.contains "update" JSHelpers.debugTraceUI then
                        printfn "Starting ExecFuncAsynch payload"
                    let cmd = func ()                    
                    return (ExecCmd cmd)})
             model, cmd'
    | ExecCmdAsynch cmd ->
        let cmd' = 
            Elmish.Cmd.OfAsyncImmediate.result (async { 
            //wavesim - 0 sleep will never update cursor in time, 100 will SOMETIMES be enough, 300 always works
            //this number only seems to affect the wavesim spinner cursor.
                do! (Async.Sleep 300)
                return (ExecCmd cmd)})
        model, cmd'
    | SendSeqMsgAsynch msgs ->
        model, doBatchOfMsgsAsynch msgs
    | MenuAction(act,dispatch) ->
        match act with 
        | MenuSaveFile -> getMenuView act model dispatch, Cmd.ofMsg (Sheet SheetT.SaveSymbols)
        | _ -> getMenuView act model dispatch, Cmd.none
        
    | DiagramMouseEvent -> model, Cmd.none
    | SetIsLoading b ->
        let cmd = if b then Cmd.none else Cmd.ofMsg (Sheet (SheetT.SetSpinner false)) //Turn off spinner after project/sheet is loaded
        {model with IsLoading = b}, cmd
    | SetGraphVisibility b ->
        {model with showGraphArea = b}, Cmd.none
    | ReadUserData userAppDir ->
        printfn $"Got user app dir of {userAppDir}"
        let model,cmd = readUserData userAppDir model        
        model,cmd
    | SetUserData (data: UserData) ->
        let model =
            {model with UserData = data}
            |> userDataToDrawBlockModel
        model, Cmd.none
    | SetThemeUserData (theme: DrawModelType.SymbolT.ThemeType) ->
        let model =
            {model with UserData = {model.UserData with Theme=theme}}
            |> userDataToDrawBlockModel
        model, Cmd.none
    | SelectionHasChanged ->
        model, Cmd.none
    | UpdateNodes -> 
        let nodeLst = model.Sheet.DCSim.NodeList
        let nodeLoc =
            [1..List.length nodeLst-1]
            |> List.map (fun i -> findConnectionsOnNode nodeLst i (BusWire.extractConnections model.Sheet.Wire))
            |> List.map (findNodeLocation)
        {model with Sheet = {model.Sheet with NodeLocations = nodeLoc}}, Cmd.none
    | UpdateVoltages newVolts -> 
        {model with Sheet = {model.Sheet with NodeVoltages = newVolts}}, Cmd.none
    | UpdateCurrents newCurrents -> 
        {model with Sheet = {model.Sheet with ComponentCurrents = newCurrents}}, Cmd.none
    | UpdateDCSim newSim ->
        {model with Sheet = {model.Sheet with DCSim=newSim}}, Cmd.none
    | UpdateACSim newSim ->
        {model with Sheet = {model.Sheet with ACSim=newSim}}, Cmd.none
    | ClearSimulationResults ->
        {model with Sheet = {model.Sheet with ACSim=[];DCSim=emptyDCResults;TimeSim=emptyTimeResults}}, Cmd.none
    | UpdateTimeSim newSim ->
        {model with Sheet = {model.Sheet with TimeSim=newSim}}, Cmd.none
    | SimulationUpdated ->
        {model with Sheet = {model.Sheet with UpdateSim=false}}, Cmd.none
    | RunSim ->
        {model with Sheet = {model.Sheet with UpdateSim=true}}, Cmd.none
    | ForceStopSim ->
        let cmd' = [HideCurrents;HideNodesOrVoltages] |> List.map Cmd.ofMsg
        {model with Sheet = {model.Sheet with SimulationRunning=false}}, Cmd.batch cmd'
    | SafeStartSim ->
        {model with Sheet = {model.Sheet with SimulationRunning=true}}, Cmd.ofMsg RunSim
    | CircuitHasErrors ->
        {model with Sheet = {model.Sheet with CanRunSimulation=false}}, Cmd.none
    | CircuitHasNoErrors ->
        {model with Sheet = {model.Sheet with CanRunSimulation=true}}, Cmd.none
    | UpdateCanvasStateSizes (compsNo,connsNo) ->
        {model with PrevCanvasStateSizes = (compsNo,connsNo)}, Cmd.ofMsg RunSim
    | ShowNodesOrVoltages ->
        let newState =
            match model.Sheet.ShowNodesOrVoltages with
            |Neither -> Nodes |Nodes -> Voltages |Voltages -> Neither
        {model with Sheet = {model.Sheet with ShowNodesOrVoltages=newState}}, Cmd.none
    | ShowOrHideCurrents ->
        {model with Sheet = {model.Sheet with ShowCurrents = (not model.Sheet.ShowCurrents)}}, Cmd.none
    | HideCurrents ->
        {model with Sheet = {model.Sheet with ShowCurrents = false}}, Cmd.none
    | HideNodesOrVoltages -> 
        {model with Sheet = {model.Sheet with ShowNodesOrVoltages = Neither}}, Cmd.none
    | ExecutePendingMessages n ->
        if n = (List.length model.Pending)
        then 
            getLastMouseMsg model.Pending
            |> function
            | None -> failwithf "shouldn't happen"
            | Some mMsg -> 
                match mMsg with
                | Sheet sMsg -> sheetMsg sMsg model
                | _ -> failwithf "shouldn't happen "
        
        //ignore the exectue message
        else 
            model, Cmd.none
    // Various messages here that are not implemented as yet, or are no longer used
    // should be sorted out
    | DoNothing -> //Acts as a placeholder to propergrate the ExecutePendingMessages message in a Cmd
        model, cmd
    | JSDiagramMsg _ | KeyboardShortcutMsg _ -> // catch all messages not otherwise processed. Should remove this?
        model, Cmd.none
    |> (fun (newModel,cmd) -> resetDialogIfSelectionHasChanged newModel oldModel,cmd)
    |> UpdateHelpers.traceMessage startOfUpdateTime msg
    |> ModelHelpers.execOneAsyncJobIfPossible
