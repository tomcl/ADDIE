(*
    ModelType.fs

    This module provides the type for the FRP UI.
    It could be put next to CommonTypes but non-UI modules should be agnostic of
    the FRP model and run independently of Fable
*)

module rec ModelType

open CommonTypes
open Fable.React
open Sheet.SheetInterface
open Optics
open MathJsHelpers

module Constants =
    /// DiagramStyle.rightSectinoWidthL = 650,
    /// WaveSimStyle.Constants.leftMargin = 50,
    /// WaveSimStyle.Constants.rightMargin = 50,
    /// 2 * MainView.Constants.dividerBarWidth = 20,
    /// WaveSimStyle.namesColWidth = 200,
    /// WaveSimStyle.valeusColWidth = 100,
    let initialWaveformColWidth = 650 - 20 - 20 - 20 - 130 - 100


/// Groups components together in the wave selection table.
/// NB: There are fields which are commented out: these can be added back in
/// later on if we want to group those components together by type rather than
/// separately by name.
type ComponentGroup =
    | WireLabel
    | InputOutput
    | Viewers
    | Buses
    | Gates
    | MuxDemux
    | Arithmetic
    | CustomComp
    | FFRegister
    | Memories
    | Component of string




type SimData = {
    ACSource: string option
    ACOutput: string option
    ACMagInDB: bool
    ACFreqInHz: bool
    TimeInput: string option
    TimeOutput: string option
    TheveninComp: string option
}

let acSource_ = Lens.create (fun a -> a.ACSource) (fun s a -> {a with ACSource= s})
let acOutput_ = Lens.create (fun a -> a.ACOutput) (fun s a -> {a with ACOutput= s})
let timeSource_ = Lens.create (fun a -> a.TimeInput) (fun s a -> {a with TimeInput = s})
let timeOutput_ = Lens.create (fun a -> a.TimeOutput) (fun s a -> {a with TimeOutput= s})
let acMag_ = Lens.create (fun a -> a.ACMagInDB) (fun s a -> {a with ACMagInDB = s})
let acFreq_ = Lens.create (fun a -> a.ACFreqInHz) (fun s a -> {a with ACFreqInHz= s})
let theveninComp_ = Lens.create (fun a -> a.TheveninComp) (fun s a -> {a with TheveninComp= s})

/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Text2 : string option;
    Text3 : string option
    Int : int option;
    Int2: int64 option
    VoltageSource: VoltageSourceType option
    ProjectPath: string
    BadLabel: bool
    VSType: string option
}

let text_ = Lens.create (fun a -> a.Text) (fun s a -> {a with Text = s})
let text2_ = Lens.create (fun a -> a.Text2) (fun s a -> {a with Text2 = s})
let text3_ = Lens.create (fun a -> a.Text3) (fun s a -> {a with Text3 = s})
let int_ = Lens.create (fun a -> a.Int) (fun s a -> {a with Int = s})
let int2_ = Lens.create (fun a -> a.Int2) (fun s a -> {a with Int2 = s})
let projectPath_ = Lens.create (fun a -> a.ProjectPath) (fun s a -> {a with ProjectPath = s})
let badLabel_ = Lens.create (fun a -> a.BadLabel) (fun s a -> {a with BadLabel= s})
let vsType_ = Lens.create (fun a -> a.VSType) (fun s a -> {a with VSType= s})


type TopMenu = | Closed | Project | Files

//==========//
// Messages //
//==========//



// Messages that will be triggered on key combinations.
type KeyboardShortcutMsg =
    | CtrlS | AltC | AltV | AltZ | AltShiftZ | DEL

type UICommandType =
    | CloseProject
    | ChangeSheet
    | RenameSheet
    | DeleteSheet
    | AddSheet
    | SaveSheet
    | StartWaveSim
    | ViewWaveSim
    | CloseWaveSim

type DiagEl = | Comp of Component | Conn of Connection

type DragMode = DragModeOn of int | DragModeOff

type IntMode = FirstInt | SecondInt

type TextMode = FirstText | SecondText | ThirdText

type MenuCommand =
    | MenuPrint
    | MenuSaveFile
    | MenuNewFile
    | MenuExit
    | MenuZoom of float


type PopupProgress =
    {
        Value: int
        Max: int
        Title: string
        Speed: float
    }

type Msg =
    | Sheet of DrawModelType.SheetT.Msg
    | JSDiagramMsg of JSDiagramMsg
    | KeyboardShortcutMsg of KeyboardShortcutMsg
    | ChangeRightTab of RightTab
    | ChangeSimSubTab of SimSubTab
    | SetHighlighted of ComponentId list * ConnectionId list
    | SetSelWavesHighlighted of ConnectionId array
    | SetClipboard of CanvasState
    | SetCreateComponent of Component
    | SetProject of Project
    | UpdateProject of (Project -> Project)
    | UpdateModel of (Model -> Model)
    | UpdateProjectWithoutSyncing of (Project->Project)
    | ShowPopup of ((Msg -> Unit) -> PopupDialogData -> ReactElement)
    | ShowStaticInfoPopup of (string * ReactElement * (Msg -> Unit))
    | ClosePopup
    | SetPopupDialogText of string option
    | SetPopupDialogText2 of string option
    | SetPopupDialogText3 of string option
    | SetPopupDialogVSType of string option
    | SetSimulationACSource of string option
    | SetSimulationACOut of string option
    | SetSimulationTimeSource of string option
    | SetSimulationTimeOut of string option
    | SetSimulationTheveninComp of string option
    | SetSimulationTheveninParams
    | SetSimulationACInDB
    | SetSimulationACInHz
    | SetPopupDialogInt of int option
    | SetPopupDialogInt2 of int64 option
    | CloseDiagramNotification
    | SetFilesNotification of ((Msg -> unit) -> ReactElement)
    | CloseFilesNotification
    | SetPopupDialogBadLabel of bool
    | SetPropertiesNotification of ((Msg -> unit) -> ReactElement)
    | ClosePropertiesNotification
    | SetTopMenu of TopMenu
    | ReloadSelectedComponent of int
    | SetDragMode of DragMode
    /// Set width of right-hand pane when tab is WaveSimulator or TruthTable
    | SetViewerWidth of int
    | MenuAction of MenuCommand * (Msg -> unit)
    | DiagramMouseEvent
    | SelectionHasChanged
    | SetIsLoading of bool
    | SetGraphVisibility of bool
    | CloseApp
    | ExecutePendingMessages of int
    | DoNothing
    | StartUICmd of UICommandType
    | FinishUICmd
    | ReadUserData of string
    | SetUserData of UserData
    | SetThemeUserData of DrawModelType.SymbolT.ThemeType
    | ExecCmd of Elmish.Cmd<Msg>
    | ExecFuncInMessage of (Model -> (Msg->Unit) -> Unit) * (Msg -> Unit)
    | ExecFuncAsynch of (Unit -> Elmish.Cmd<Msg>)
    | ExecCmdAsynch of Elmish.Cmd<Msg>
    | SendSeqMsgAsynch of seq<Msg>
    | UpdateNodes 
    | UpdateCurrents of Map<ComponentId,float>
    | UpdateVoltages of float list
    | UpdateDCSim of DCSimulationResults
    | UpdateACSim of ComplexP list
    | UpdateTimeSim of TimeSimulationResults
    | ShowNodesOrVoltages
    | ShowNodesOrVoltagesExplicitState of DCNodesOrVoltagesOrNone
    | HideNodesOrVoltages
    | SimulationUpdated
    | RunSim
    | ForceStopSim
    | CircuitHasErrors
    | CircuitHasNoErrors
    | SafeStartSim
    | ShowOrHideCurrents
    | HideCurrents
    | ClearSimulationResults
    | UpdateCanvasStateSizes of int*int
    | UpdateDiodeModes of bool list
    | RunTests


//================================//
// Componenents loaded from files //
//================================//

type Notifications = {
    FromDiagram : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromSimulation : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromWaveSim : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromFiles : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromMemoryEditor : ((Msg -> unit) -> Fable.React.ReactElement) option
    FromProperties : ((Msg -> unit) -> Fable.React.ReactElement) option
}

type UserData = {
    /// Where to save the persistent app data
    UserAppDir : string option
    LastUsedDirectory: string option
    RecentProjects: string list option
    WireType: DrawModelType.BusWireT.WireType
    Theme: DrawModelType.SymbolT.ThemeType
    }

type SpinnerState =
   | WaveSimSpinner

type SpinPayload = {
    Payload: Model -> Model
    Name: string
    ToDo: int
    Total: int
    }



type Model = {
    UserData: UserData
    
    /// If the application has a modal spinner waiting for simulation
    Spinner: (Model -> Model) option
        
    /// Draw Canvas
    Sheet: DrawModelType.SheetT.Model

    /// true during period when a sheet or project is loading
    IsLoading: bool

    /// last time check for changes was made
    LastChangeCheckTime: float

    /// top-level canvas used for current wave simulation
    LastSimulatedCanvasState: CanvasState option // reduced (without layout) canvas state
    /// used to determine whether current canvas has been saved (includes any change)
    LastDetailedSavedState: CanvasState
    /// components and connections currently selected

    CurrentSelected: Component list * Connection list
    /// component ids and connection ids previously selected (used to detect changes)
    LastSelectedIds: string list * string list
    /// last used bus width in bits - used as default in next component create dialog
    LastUsedDialogWidth: int
    /// component currently selected in properties dialog
    SelectedComponent : Component option // None if no component is selected.
    /// which of the tabbed panes is currently visible
    RightPaneTabVisible : RightTab
    /// which of the subtabs for the right pane simulation is visible
    SimSubTabVisible: SimSubTab
    /// components and connections which are highlighted
    Hilighted : (ComponentId list * ConnectionId list) * ConnectionId list
    /// Components and connections that have been selected and copied.
    Clipboard : CanvasState 
    /// Track the last added component
    LastCreatedComponent : Component option 
    /// used to enable "SAVE" button
    SavedSheetIsOutOfDate : bool
    /// the project contains, as loadable components, the state of each of its sheets
    CurrentProj : Project option
    /// function to create popup pane if present
    PopupViewFunc : ((Msg -> Unit) -> PopupDialogData -> Fable.React.ReactElement) option
    /// function to create spinner popup pane if present (overrides otehr popups)
    SpinnerPayload : SpinPayload option
    /// data to populate popup (may not all be used)
    PopupDialogData : PopupDialogData
    /// record containing functions that create react elements of notifications
    Notifications : Notifications
    /// State of menus for sheets, projects etc
    TopMenuOpenState : TopMenu
    /// used to determine whether mouse is currently dragging the divider, or used normally
    DividerDragMode: DragMode
    /// Contains a list of pending messages
    Pending: Msg list
    UIState: UICommandType Option
    /// Controls the visibility of the graph area (bottom section of the screen)
    showGraphArea: bool
    /// Contains the necessary information to run all simulations
    SimulationData: SimData
    /// Contains the lengths of the Component and Connection lists, to break simulation if they change
    PrevCanvasStateSizes: int*int
    /// Contains the cached diode modes of the previous simulation 
    PreviousDiodeModes: bool list
    /// Contains the outcome of the tests
    Tests: bool list
    /// Contains the thevenin parameters of a particular circuit
    TheveninParams: TheveninParameters option
} 

    
let sheet_ = Lens.create (fun a -> a.Sheet) (fun s a -> {a with Sheet = s})
let popupDialogData_ = Lens.create (fun a -> a.PopupDialogData) (fun p a -> {a with PopupDialogData = p})
let simulationData_ = Lens.create (fun a -> a.SimulationData) (fun p a -> {a with SimulationData = p})
let currentProj_ = Lens.create (fun a -> a.CurrentProj) (fun s a -> {a with CurrentProj = s})


    