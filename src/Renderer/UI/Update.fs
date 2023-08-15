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
open Sheet.SheetInterface
open DrawModelType
open Fable.SimpleJson
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
            let jsonRes = tryReadFileSync <| pathJoin [|userAppDir;"AddieSettings.json"|]
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
        |> Result.bind (fun json -> writeFile (pathJoin [|userAppDir;"AddieSettings.json"|]) json)
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
        FileMenuView.doActionWithSaveFileDialog "Exit Addie" CloseApp model dispatch ()
    
            
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


//---------------------------PROJECT UPDATE AND SAVING----------------------------//

/// Save any changed sheets to disk in the project directory
let syncLoadedComponentsToDisk newProj oldProj =
    let needToSave ldc' ldc =
       (not <| compareCanvas 10. ldc'.CanvasState ldc.CanvasState)
    let saveToDisk ldc =
        let state = ldc.CanvasState
        let sheetInfo = {Form=ldc.Form;Description=ldc.Description}
        saveStateToFile newProj.ProjectPath ldc.Name (state,sheetInfo)
        |> ignore
        removeFileWithExtn ".dgmauto" oldProj.ProjectPath ldc.Name

    let nameOf sheet (ldc:LoadedComponent) = ldc.Name = sheet
    let oldLDCs = oldProj.LoadedComponents
    let newLDCs = newProj.LoadedComponents
    let sheets = List.distinct (List.map (fun ldc -> ldc.Name) (oldLDCs @ newLDCs))
    let sheetMap = 
        sheets
        |> List.map (fun sheet -> sheet, (List.tryFind (nameOf sheet) newLDCs, List.tryFind (nameOf sheet) oldLDCs))
        |> Map.ofList
    sheetMap
    |> Map.iter (fun name (optLdcNew, optLdcOld) ->
        match optLdcNew,optLdcOld with
        | Some ldcNew, Some ldcOld when needToSave ldcNew ldcOld ->
            saveToDisk ldcOld
        | Some _, Some _ -> ()
        | None, Some ldcOld -> 
            removeFileWithExtn ".dgm" oldProj.ProjectPath ldcOld.Name
        | Some ldcNew, None -> 
            saveToDisk ldcNew
        | None, None -> failwithf "What? Can't happen")

/// Return new model with project updated as per update function.
/// If p.LoadedComponents data is changed, for each sheet that is different
/// the sheet will be saved to disk.
/// This function should be used consistently to keep disk and project data
/// correct.
let updateProjectFiles (saveToDisk:bool) (update: Project -> Project) (model: Model) =
    match model.CurrentProj with
    | None -> model // do nothing in this case
    | Some p ->
        let p' = update p
        if saveToDisk then 
            syncLoadedComponentsToDisk p' p // write out to disk as needed
        {model with CurrentProj = Some p'}


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
        {model with UIState = None;}, Cmd.ofMsg (Sheet (SheetT.SetSpinner false))
    | CloseApp ->
        exitApp model
        model, Cmd.none
    | Sheet sMsg ->
        match sMsg, model.PopupViewFunc with
        | SheetT.ToggleNet canvas, _ ->
            model, Cmd.none
        | SheetT.KeyPress _, Some _ -> 
            // do not allow keys to affect Sheet when popup is on.
            model, Cmd.none
        | _ -> sheetMsg sMsg model
    // special mesages for mouse control of screen vertical dividing bar, active when Wavesim is selected as rightTab
    | ChangeRightTab newTab -> 
        let inferMsg = JSDiagramMsg <| InferWidths()
        let editCmds = [inferMsg; ClosePropertiesNotification] |> List.map Cmd.ofMsg
        firstTip <- true
        { model with RightPaneTabVisible = newTab }, Cmd.batch <| editCmds
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
    //| SetCreateComponent pos -> { model with LastCreatedComponent = Some pos }, Cmd.none
    | SetProject project ->
        model
        |> set currentProj_ (Some project) 
        |> set (popupDialogData_ >-> projectPath_) project.ProjectPath, Cmd.none
    | UpdateProjectWithoutSyncing update -> 
        updateProjectFiles false update model,Cmd.none
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
    | SetSimulationTheveninParams ->
        let comps,conns = model.Sheet.GetCanvasState() |> Simulation.combineGrounds
        let nodeLst = CanvasStateAnalyser.createNodetoCompsList (comps,conns)
        match model.SimulationData.TheveninComp with
        |Some id when id<>"sel" ->
            let pair = findNodesOfComp nodeLst id
            match pair with 
            |Some (p1,p2) ->
                let thevP = Simulation.findTheveninParams (comps,conns) id p1 p2
              //  {model with TheveninParams = Some thevP}, Cmd.none
                match thevP with
                 | Error m -> 
                    set (popupDialogData_ >-> text_) (Some m) model, Cmd.none
                 | Ok value -> {model with TheveninParams = Some value}, Cmd.none
            // the Cmd will always be none from the return value so this is faster
            |_ -> model, Cmd.none
        |_ -> model, Cmd.none
    | SetSimulationTheveninComp c ->
        set (simulationData_ >-> theveninComp_) c model, Cmd.none
    | SetPopupDialogBadLabel isBad ->
        set (popupDialogData_ >-> badLabel_) isBad model, Cmd.none
    | SetPopupDialogInt int ->
        set (popupDialogData_ >-> int_) int model, Cmd.none
    | SetPopupDialogInt2 int ->
        set (popupDialogData_ >-> int2_) int model, Cmd.none
    | CloseDiagramNotification ->
        { model with Notifications = {model.Notifications with FromDiagram = None} }, Cmd.none
    | CloseFilesNotification ->
        { model with Notifications = {model.Notifications with FromFiles = None} }, Cmd.none
    | ClosePropertiesNotification ->
        { model with Notifications = { model.Notifications with FromProperties = None} }, Cmd.none
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
    | ClearSimulationResults ->
        {model with TheveninParams = None; Sheet = {model.Sheet with ACSim=[];DCSim=emptyDCResults;TimeSim=emptyTimeResults}}, Cmd.none
    | ForceStopSim ->
        let cmd' = [UpdateSheet ("Changing the sheet", (Optic.set ShowCurrents_ false)); UpdateSheet ("Changing the sheet", (Optic.set ShowNodesOrVoltages_ Neither)); UpdateModelNew ("setting graph visibility", Optic.set showGraphArea_ false);ClearSimulationResults] |> List.map Cmd.ofMsg
        {model with Sheet = {model.Sheet with SimulationRunning=false}}, Cmd.batch cmd'
    | SafeStartSim ->
        {model with Sheet = {model.Sheet with SimulationRunning=true}}, Cmd.none
    | CircuitHasErrors ->
        {model with Sheet = {model.Sheet with CanRunSimulation=false}}, Cmd.none
    | CircuitHasNoErrors ->
        let cmd' = [(Sheet (DrawModelType.SheetT.Msg.Wire (BusWireT.Msg.ErrorWires []) ));(Sheet (DrawModelType.SheetT.Msg.Wire (BusWireT.Msg.Symbol (SymbolT.Msg.ErrorSymbols ([],[],false) ))))]
        {model with Sheet = {model.Sheet with CanRunSimulation=true}}, cmd' |> List.map Cmd.ofMsg |> Cmd.batch
    | RunTests ->
        let tests = Test.runTestCases ()
        {model with Tests = tests}, Cmd.none
    | UpdateCanvasStateSizes (compsNo,connsNo) ->
        {model with PrevCanvasStateSizes = (compsNo,connsNo)}, Cmd.none
    | ShowNodesOrVoltages ->
        let newState =
            match model.Sheet.ShowNodesOrVoltages with
            |Neither -> Nodes |Nodes -> Voltages |Voltages -> Neither
        {model with Sheet = {model.Sheet with ShowNodesOrVoltages=newState}}, Cmd.none
    | ShowOrHideCurrents ->
        {model with Sheet = {model.Sheet with ShowCurrents = (not model.Sheet.ShowCurrents)}}, Cmd.none
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
    | UpdateModelNew (s, f) ->
        f model, Cmd.none
    | UpdateNotification (s, nf) ->
        (Optic.set Notifications_ (nf model.Notifications) model), Cmd.none
    | UpdateSheet (s, nf) ->
        (Optic.set Sheet_ (nf model.Sheet) model), Cmd.none
    | DoNothing -> //Acts as a placeholder to propergrate the ExecutePendingMessages message in a Cmd
        model, cmd
    | JSDiagramMsg _ | KeyboardShortcutMsg _ -> // catch all messages not otherwise processed. Should remove this?
        model, Cmd.none
    |> (fun (newModel,cmd) -> resetDialogIfSelectionHasChanged newModel oldModel,cmd)
    |> UpdateHelpers.traceMessage startOfUpdateTime msg
    |> ModelHelpers.execOneAsyncJobIfPossible
