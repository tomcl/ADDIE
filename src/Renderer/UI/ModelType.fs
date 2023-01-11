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



type RightTab =
    | Properties
    | Catalogue
    | Simulation

type SimSubTab =
    | DCsim
    | ACsim
    | TimeSim


/// Possible fields that may (or may not) be used in a dialog popup.
type PopupDialogData = {
    Text : string option;
    Int : int option;
    Int2: int64 option
    ProjectPath: string
    BadLabel: bool
}

let text_ = Lens.create (fun a -> a.Text) (fun s a -> {a with Text = s})
let int_ = Lens.create (fun a -> a.Int) (fun s a -> {a with Int = s})
let int2_ = Lens.create (fun a -> a.Int2) (fun s a -> {a with Int2 = s})
let projectPath_ = Lens.create (fun a -> a.ProjectPath) (fun s a -> {a with ProjectPath = s})
let badLabel_ = Lens.create (fun a -> a.BadLabel) (fun s a -> {a with BadLabel= s})

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
    | SetPopupDialogInt of int option
    | SetPopupDialogInt2 of int64 option
    | SetPopupDialogTwoInts of (int64 option * IntMode * string option)
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
    WaveSimViewerWidth: int
    ConnsOfSelectedWavesAreHighlighted: bool
} 

    
let sheet_ = Lens.create (fun a -> a.Sheet) (fun s a -> {a with Sheet = s})
let popupDialogData_ = Lens.create (fun a -> a.PopupDialogData) (fun p a -> {a with PopupDialogData = p})
let currentProj_ = Lens.create (fun a -> a.CurrentProj) (fun s a -> {a with CurrentProj = s})


    