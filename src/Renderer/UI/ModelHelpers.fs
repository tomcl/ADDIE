module ModelHelpers
open CommonTypes
open Sheet.SheetInterface
open ModelType
open Elmish


/// This is needed because DrawBlock cannot directly access Issie Model.
/// can be replaced when all Model is placed at start of compile order and DB
/// model is refactored
let drawBlockModelToUserData (model: Model) (userData: UserData)=
    let bwModel =model.Sheet.Wire
    {userData with WireType = bwModel.Type;}

/// This is needed because DrawBlock cannot directly access Issie Model.
/// can be replaced when all Model is placed at start of compile order and DB
/// model is refactored
let userDataToDrawBlockModel (model: Model) =
    let userData = model.UserData
    {model with 
        Sheet = 
            {model.Sheet with 
                Wire = {
                    model.Sheet.Wire with 
                        Type = userData.WireType
                        Symbol = {
                            model.Sheet.Wire.Symbol with Theme = userData.Theme
                        }}}}

let reduce (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Hilighted = this.Hilighted
         Clipboard = this.Clipboard
         LastSimulatedCanvasState = this.LastSimulatedCanvasState
         LastSelectedIds = this.LastSelectedIds
         CurrentSelected = this.CurrentSelected
         LastUsedDialogWidth = this.LastUsedDialogWidth
         SelectedComponent= this.SelectedComponent
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         PopupDialogData = this.PopupDialogData
         TopMenu = this.TopMenuOpenState
         DragMode = this.DividerDragMode

 |} 
       
let reduceApprox (this: Model) = {|
         RightTab = this.RightPaneTabVisible
         Clipboard = this.Clipboard
         CurrProject = match this.PopupViewFunc with None -> false | _ -> true
         LastUsedDialogWidth = this.LastUsedDialogWidth
         CreateComponent = this.LastCreatedComponent
         HasUnsavedChanges = false
         PopupDialogData = this.PopupDialogData
         DragMode = this.DividerDragMode
 |} 

let mapFst mapFn (model,cmd) = mapFn model, cmd

let mapOverProject defaultValue (model: Model) transform =
    match model.CurrentProj with
    | None -> defaultValue
    | Some p -> transform p

let getComponentIds (model: Model) =
    let extractIds ((comps,conns): Component list * Connection list) = 
        conns
        |> List.map (fun comp -> ComponentId comp.Id)
        
    model.Sheet.GetCanvasState()
    |> extractIds
    |> Set.ofList


//----------------------Print functions-----------------------------//
//------------------------------------------------------------------//

let spComp (comp:Component) =
    match comp.Type with
    | Custom {Name=name; IOLabels=io} -> sprintf "Custom:%s(ins=%A)" name io 
    | x -> sprintf "%A" x

let spConn (conn:Connection) = 
    sprintf "Conn:%A" conn.Vertices

let spState ((comps,conns):CanvasState) = 
    sprintf "Canvas<%A,%A>" (List.map spComp comps) (List.map spConn conns)

let spCanvas (model : Model) = 
    model.Sheet.GetCanvasState()
    |> spState

let spComps comps =  
    sprintf "Comps%A" (List.map spComp comps)

let spOpt f thingOpt = match thingOpt with |None -> "None" | Some x -> sprintf "Some %s" (f x)

let spLdComp (ldc: LoadedComponent) =
    sprintf "LDC<%s:%A:%s>" ldc.Name ldc.TimeStamp ((fst >>spComps) ldc.CanvasState)

let spProj (p:Project) =
    sprintf "PROJ||Sheet=%s\n%s||ENDP\n" p.OpenFileName (String.concat "\n" (List.map spLdComp p.LoadedComponents))

let pp model =
    printf "\n%s\n%s" (spCanvas model) (spOpt spProj model.CurrentProj)

let spMess msg =
    match msg with
    //| SetProject p -> sprintf "MSG<<SetProject:%s>>ENDM" (spProj p)
    //| SetLastSimulatedCanvasState canvasOpt-> sprintf "MSG<SetLastSimCanv:%s>>ENDM" (spOpt spState canvasOpt)
    | x -> sprintf "MSG<<%20A>>ENDM" x

let tryGetLoadedComponents model =
    match model.CurrentProj with
    | Some p -> p.LoadedComponents
    | _ -> []

let updateLdComps (name:string) (changeFun: LoadedComponent -> LoadedComponent)  (ldComps: LoadedComponent list)=
    ldComps
    |> List.map (fun ldc -> if ldc.Name=name then changeFun ldc else ldc)

let updateLdCompsWithCompOpt (newCompOpt:LoadedComponent option) (ldComps: LoadedComponent list) =
    match newCompOpt with 
    | None -> ldComps // no update
    | Some newComp -> 
        match List.tryFind (fun (ldc:LoadedComponent) -> ldc.Name = newComp.Name) ldComps with
        | None -> newComp :: ldComps
        | Some _ -> updateLdComps newComp.Name (fun _ -> newComp) ldComps

/// returns a string option representing the current file name if file is loaded, otherwise None
let getCurrFile (model: Model) =
    match model.CurrentProj with
    | Some proj -> Some proj.OpenFileName
    | None -> None

let getCurrSheets (model: Model) =
    match model.CurrentProj with
    | Some proj -> 
        proj.LoadedComponents
        |> List.map (fun lc -> lc.Name)
        |> Some
    | None -> None


/// a long function to be executed in a message after the view function has run at least once
type ViewableJob = {
    JobWork: Model-> Model * Cmd<Msg>
    ViewHasExecuted: bool
    JobName: string
    }

/// list of jobs awaiting execution
let mutable asyncJobs: ViewableJob list = []

let runAfterView (jobName:string) ( workFn: Model -> Model * Cmd<Msg>) =
    let job = {JobWork=workFn; ViewHasExecuted = false; JobName = jobName}
    printfn $"scheduling {jobName}"
    asyncJobs <- List.append asyncJobs [job]

let setAsyncJobsRunnable dispatch =
    dispatch DoNothing
    if asyncJobs.Length > 0 then 
        printfn "setting asynch jobs to vieHasExecuted"
    asyncJobs <- 
        asyncJobs 
        |> List.map (fun job -> {job with ViewHasExecuted = true}); 

/// called from update function, it will execute outstanding async jobs.
/// each job modifies model.
let execOneAsyncJobIfPossible (model: Model,cmd: Cmd<Msg>)=
    asyncJobs
    |> List.filter (fun job -> job.ViewHasExecuted) 
    |> function 
        | [] -> (model,cmd)
        | job::_ -> 
            asyncJobs <- List.filter (fun job' -> job'.JobName <> job.JobName) asyncJobs 
            printfn $"Executing async '{job.JobName}."
            job.JobWork model
            |> (fun (model', cmd') -> model', Cmd.batch [cmd; cmd'])

