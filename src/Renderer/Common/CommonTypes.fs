(*
    These are types used throughout the application
*)

module CommonTypes
    open Fable.Core               
    open Optics
    /// Position on SVG canvas
    /// Positions can be added, subtracted, scaled using overloaded +,-, *  operators
    /// currently these custom operators are not used in Issie - they should be!
    type XYPos =
        {
            X : float
            Y : float
        }
    
        /// allowed tolerance when comparing positions with floating point errors for equality
        static member inline epsilon = 0.0000001
    
        /// Add postions as vectors (overlaoded operator)
        static member inline ( + ) (left: XYPos, right: XYPos) =
            { X = left.X + right.X; Y = left.Y + right.Y }
    
        /// Subtract positions as vectors (overloaded operator)
        static member inline ( - ) (left: XYPos, right: XYPos) =
            { X = left.X - right.X; Y = left.Y - right.Y }
    
        /// Scale a position by a number (overloaded operator).
        static member inline ( * ) (pos: XYPos, scaleFactor: float) =
            { X = pos.X*scaleFactor; Y = pos.Y * scaleFactor }
    
        /// Compare positions as vectors. Comparison is approximate so 
        /// it will work even with floating point errors. New infix operator.
        static member inline ( =~ ) (left: XYPos, right: XYPos) =
            abs (left.X - right.X) <= XYPos.epsilon && abs (left.Y - right.Y) <= XYPos.epsilon
    
    let inline euclideanDistance (pos1: XYPos) (pos2:XYPos) = 
        let vec = pos1 - pos2
        sqrt(vec.X**2 + vec.Y**2)
    
    /// example use of comparison operator: note that F# type inference will not work without at least
    /// one of the two operator arguments having a known XYPos type.
    let private testXYPosComparison a  (b:XYPos) = 
        a =~ b   
  

    //==========================================//
    // Canvas state mapped to f# data structure //
    //==========================================//

    /// Specify the type of a port in a Component.
    type PortType = Input | Output

    (*
    Note on Ports. Ports are used throughout Issie to represent I/Os of components.
    Because a design sheet can be instantiated as a component they can also represent I/Os of a sheet.

    1. Port records are used on both connections and components, a connection
       source or target port will have port Id matching that of the port on the
       component it connects to. All ports also specify the componentId of the
       component they are on (HostID).
    2. Port records on connections do NOT have port numbers, note this means that connection ports
       cannot be the same as the corresponding component port.
    3. Port numbers on components are contiguous from 0 separtely for input
       and output ports.
    4. Port numbers must match with the index of the port in the corresponding component
       InputPorts or OutputPorts list
    5. For custom components port numbers match index of the port in InputPortNames,OutputPortNames
    6. For symbols port numbers determine the vertical order in which ports are displayed.
    7. Thus when changing the order of number of I/Os on a custom component port numbers can be changed
       as long as port lists and port name lists are similarly re-ordered.
    8. In the simulation port numbers are not relevant for custom comps - connections match port names with the 
       sheet input or output component for the port
    9. In the simulation port numbers matter for all other ports: the simulator defines operation based on them.
    10.In model.Symbol ports are kept in a single global map, including port numbers. If port numbers are permuted on
       custom components the port numbers in this map must be changed. However this will normally happen since
       model.Symbol symbols and ports are changed at the same time by AddSymbol or deleteSymbol or LoadComponents
       messages.
    *)


    /// A component I/O.
    ///
    /// Id (like any other Id) is a string generated with 32 random hex charactes,
    /// so it is (practically) globally unique. These Ids are used 
    /// to uniquely refer to ports and components. They are generated via uuid().
    ///
    /// PortNumber is used to identify which port is which on a component, contiguous from 0
    /// separately for inputs and outputs. See comments above type definition for details
    ///
    /// HostId is the unique Id of the component where the port is. For example,
    /// all three ports on the same And component will have the same HostId.
    type Port = {
        Id : string
        // For example, an And would have input ports 0 and 1, and output port 0.
        // If the port is used in a Connection record as Source or Target, the Number is None. 
        PortNumber : int option 
        HostId : string
    }

    type PortId = | PortId of string

    // NB - this.Text() is not currently used.

    /// This width is for wire displaying, >8 buswires displayed with 8px thickness. Actual size stored in Port type
    type Width = One | Two | Three | Four | Five | Six | Seven | Eight
    with
        member this.Text() = // the match statement is used for performance
            match this with
            | One -> "1px"
            | Two -> "2px"
            | Three -> "3px"
            | Four -> "4px"
            | Five -> "5px"
            | Six -> "6px"
            | Seven -> "7px"
            | Eight -> "8px"
            
            
    /// Type to specify the origin of a custom component
    type CCForm =
        |User
        |Library
        |ProtectedTopLevel
        |ProtectedSubSheet


    /// Name identifies the LoadedComponent used.
    /// The labels define legends on symbol designating inputs or outputs: and are the names of the Input or Output components of the CC sheet.
    /// Label strings are unique per CustomComponent.
    /// Label position in list determines inputPortNumber or outputPortNumber of label.
    /// Multiple CustomComponent instances are differentiated by Component data.
    type CustomComponentType = {
        Name: string
        IOLabels: string  list
        Form : CCForm option
        Description : string option
    }

    /// Note that any memory addresses which have not been explicitly set when printing
    /// out memory data.
    type Memory = {
        // How many bits the address should have.
        // The memory will have 2^AddressWidth memory locations.
        AddressWidth : int 
        // How wide each memory word should be, in bits.
        WordWidth : int
        // Data is a list of <2^AddressWidth> elements, where each element is a
        // 64 bit integer. This makes words longer than 64 bits not supported.
        // This can be changed by using strings instead of int64, but that is way
        // less memory efficient.
        Data : Map<int64,int64>
    }

    type InitMemData = 
        | FromData // old method (from data field)
        | FromFile of string // FromFile fName => read a file fName.ram for data
        | ToFile of string // ToFile fName => write data to a file fName.ram
        | ToFileBadName of string // as ToFile but the name does not validate
        | UnsignedMultiplier
        | SignedMultiplier


    type Memory1 = {
    // is the data initialised from a file name.ram in the project directory, or some other way?
    Init: InitMemData
    // How many bits the address should have.
    // The memory will have 2^AddressWidth memory locations.
    AddressWidth : int 
    // How wide each memory word should be, in bits.
    WordWidth : int
    // Data is a list of <2^AddressWidth> elements, where each element is a
    // 64 bit integer. This makes words longer than 64 bits not supported.
    // This can be changed by using strings instead of int64, but that is way
    // less memory efficient.
    Data : Map<int64,int64>  
    } 

    type VoltageSourceType =
        |DC of Voltage:float
        |ACAnalysis of Amplitude:float * Phase:float
        |Sine of Amplitude:float * Offset:float * Frequency:float
        |Pulse of V1:float * V2:float * Frequency:float
    
    // Types instantiating objects in the Digital extension.
    type ComponentType =
        | IOLabel
        | IO
        | Resistor of Resistance:float * TextBoxValue:string
        | Capacitor of Capacitance:float * TextBoxValue:string
        | Inductor of Inductance:float * TextBoxValue:string
        | VoltageSource of VoltageSourceType
        | CurrentSource of Current:float * TextBoxValue:string
        | Diode
        | Ground
        | Opamp
        | Custom of CustomComponentType // schematic sheet used as component



    // --------------- Types needed for symbol ---------------- //
    /// Represents the rotation of a symbol in degrees, Degree0 is the default symbol rotation.
    /// Angle is anticlockwise
    type Rotation = | Degree0 | Degree90 | Degree180 | Degree270
    
    /// Stores the rotation and the flip of the symbol, flipped false by default
    type STransform = {Rotation: Rotation; flipped: bool}
    
    /// Represents the sides of a component
    type Edge = | Top | Bottom | Left | Right

    type BoundingBox = {
        /// Top left corner of the bounding box
        TopLeft: XYPos
        /// Width
        W: float
        /// Height
        H: float
    }
        with member this.Centre() = this.TopLeft + {X=this.W/2.; Y=this.H/2.}
   
   
    
    type ScaleAdjustment =
        | Horizontal
        | Vertical
    
    type SymbolInfo = {
        LabelBoundingBox: BoundingBox option
        LabelRotation: Rotation option
        STransform: STransform
        PortOrientation: Map<string, Edge>
        PortOrder: Map<Edge, string list>
        HScale: float option
        VScale: float option
    }



    let getSTransformWithDefault (infoOpt: SymbolInfo option) =
        match infoOpt with
        | None ->{Rotation=Degree0; flipped=false}
        | Some inf -> inf.STransform

    module LegacyCanvas =
        /// JSComponent mapped to F# record.
        /// Id uniquely identifies the component within a sheet.
        /// Label is optional descriptor displayed on schematic.
        type LegacyComponent = {
            Id : string
            Type : ComponentType
            Label : string // All components have a label that may be empty.
            InputPorts : Port list // position on this list determines inputPortNumber
            OutputPorts : Port list // position in this lits determines OutputPortNumber
            X : float
            Y : float
            H : float
            W : float
        }

        /// JSConnection mapped to F# record.
        /// Id uniquely identifies connection globally and is used by library.
        type LegacyConnection = {
            Id : string
            Source : Port
            Target : Port
            Vertices : (float * float) list
        }

        /// F# data describing the contents of a single schematic sheet.
        type LegacyCanvasState = LegacyComponent list * LegacyConnection list


    /// JSComponent mapped to F# record.
    /// Id uniquely identifies the component within a sheet.
    /// Label is optional descriptor displayed on schematic.
    type Component = {
        Id : string
        Type : ComponentType
        Label : string // All components have a label that may be empty.
        IOPorts : Port list // position on this list determines inputPortNumber
        X : float
        Y : float
        H : float
        W : float
        SymbolInfo : SymbolInfo option
    }

    with member this.getPort (PortId portId: PortId) = 
            List.tryFind (fun (port:Port) -> port.Id = portId ) (this.IOPorts)
     
     
    let type_ = Lens.create (fun c -> c.Type) (fun n c -> {c with Type = n})
    let ioPorts_ = Lens.create (fun c -> c.IOPorts) (fun n c -> {c with IOPorts = n})
    let h_ = Lens.create (fun c -> c.H) (fun n c -> {c with H= n})
    let w_ = Lens.create (fun c -> c.W) (fun n c -> {c with W= n})

    /// JSConnection mapped to F# record.
    /// Id uniquely identifies connection globally and is used by library.
    type Connection = {
        Id : string
        Source : Port
        Target : Port
        Vertices : (float * float * bool) list
    }

    /// F# data describing the contents of a single schematic sheet.
    type CanvasState = Component list * Connection list

    

    /// reduced version of CanvasState for electrical comparison, all geometry removed, components ordered
    type ReducedCanvasState = | ReducedCanvasState of CanvasState

    let unreduced (ReducedCanvasState(rComps,rConns)) = rComps,rConns



    //=======//
    // Other //
    //=======//

    ///unconfigured replaces Some -1, Error replaces None, Configured of int replaces Some (positive int)
    type WireWidth = | Configured of int | Unconfigured | ErrorWidth

    type NumberBase = | Hex | Dec | Bin | SDec

    /// Colors to highlight components
    /// Case name is used as HTML color name.
    /// See JSHelpers.getColorString
    /// lots of colors can be added, see https://www.w3schools.com/colors/colors_names.asp
    /// The Text() method converts it to the correct HTML string
    /// Where speed matters the color must be added as a case in the match statement
    type HighLightColor = Red | Blue | Yellow | Green | Orange | Grey | White | Purple | DarkSlateGrey | Thistle | Brown |SkyBlue
    with 
        member this.Text() = // the match statement is used for performance
            match this with
            | Red -> "Red"
            | Blue -> "Blue"
            | SkyBlue -> "Skyblue"
            | Yellow -> "Yellow"
            | Green -> "Green"
            | Grey -> "Grey"
            | Purple -> "Purple"
            | DarkSlateGrey -> "darkslategrey"
            | Thistle -> "thistle"
            | c -> sprintf "%A" c
            
    let omegaString = "\u03a9"

    // The next types are not strictly necessary, but help in understanding what is what.
    // Used consistently they provide type protection that greatly reduces coding errors

    /// SHA hash unique to a component - common between JS and F#
    [<Erase>]
    type ComponentId = | ComponentId of string

    /// Unique identifier for a fast component.
    /// The list is the access path, a list of all the containing custom components 
    /// from the top sheet of the simulation (root first)
    type FComponentId = ComponentId * ComponentId list

    /// SHA hash unique to a connection - common between JS and F#
    [<Erase>]
    type ConnectionId     = | ConnectionId of string

    /// type to uniquely identify a segment
    type SegmentId      = int * ConnectionId


    /// Human-readable name of component as displayed on sheet.
    /// For I/O/labelIO components a width indication eg (7:0) is also displayed, but NOT included here
    [<Erase>]
    type ComponentLabel   = | ComponentLabel of string

    /// SHA hash unique to a component port - common between JS and F#.
    /// Connection ports and connected component ports have the same port Id
    [<Erase>]
    type IOPortId      = | PortId of string

    
    /// Port numbers are sequential unique with port lists.
    /// Inputs and Outputs are both numberd from 0 up.
    [<Erase>]
    type InputPortNumber  = | InputPortNumber of int

    /// Port numbers are sequential unique with port lists.
    /// Inputs and Outputs are both numberd from 0 up.
    [<Erase>]
    type OutputPortNumber = | OutputPortNumber of int

    
    (*-----------------------------------------------------------------------------*)
    // Types used within waveform Simulation code, and for saved wavesim configuartion

    

    /// Info regarding sheet saved in the .dgm file
    type SheetInfo = {
        Form: CCForm option 
        Description: string option
    }

    (*--------------------------------------------------------------------------------------------------*)

    /// Static data describing a schematic sheet loaded as a custom component.
    /// Every sheet is always identified with a file from which it is loaded/saved. 
    /// Name is human readable (and is the filename - without extension) and identifies sheet.
    /// File path is the sheet directory and name (with extension).
    /// InputLabels, OutputLabels are the I/O connections.
    /// The I/O connection integers are bus widths.
    /// The I/O connection strings are human readable. The strings are guaranteed
    /// to be unique in the I/O connection list. I.e. An input label may be the same
    /// as an output label, but two input (or output) labels cannot be the same.
    /// The position in the I/O connections list is important as it implicitly
    /// indicates the port number. For example, the first element in the InputLabels
    /// list is related to the Component's Port with PortNumber 0.
    /// Two instances of a loaded component have the same LoadedComponent data.
    type LoadedComponent = {
        /// File name without extension = sheet name
        Name: string
        /// When the component was last saved
        TimeStamp: System.DateTime 
        /// Complete file path, including name and dgm extension
        FilePath : string
        /// F# equivalent of Diagram components and connections including layout
        CanvasState : CanvasState
        /// IO port names, and port numbers in any created custom component
        IOLabels : string list
        Form : CCForm option
        Description: string option
    }

    open Optics.Operators

    let canvasState_ = Lens.create (fun a -> a.CanvasState) (fun s a -> {a with CanvasState = s})
    let componentsState_ = canvasState_ >-> Optics.fst_


    /// Type for an open project which represents a complete design.
    /// ProjectPath is directory containing project files.
    /// OpenFileName is name of file from which current schematic sheet is loaded/saved, without extension or path
    /// LoadedComponents contains the list of schematic sheets, each as a component, one per sheet.
    type Project = {
        /// directory which contains the project files
        ProjectPath : string
        /// name of viewed sheet (Form: User) (without extension)
        OpenFileName : string
        /// name of sheet performing operation on (e.g.: when Verilog Editor is open)
        WorkingFileName : string option
        /// componnets have one-one correspondence with files
        LoadedComponents : LoadedComponent list
        }

        

    let loadedComponents_ = Lens.create (fun a -> a.LoadedComponents) (fun s a -> {a with LoadedComponents = s})

    let openLoadedComponent_ = 
        Lens.create 
            (fun a -> List.find (fun lc -> lc.Name = a.OpenFileName) a.LoadedComponents) 
            (fun lc' a -> {a with LoadedComponents = List.map (fun lc -> if lc.Name = a.OpenFileName then lc' else lc) a.LoadedComponents})

    let openFileName_ = Lens.create (fun a -> a.OpenFileName) (fun s a -> {a with OpenFileName = s})

    let loadedComponentOf_ (name:string) = 
        Lens.create 
            (fun a -> List.find (fun lc -> lc.Name = name) a.LoadedComponents) 
            (fun lc' a -> {a with LoadedComponents = List.map (fun lc -> if lc.Name = name then lc' else lc) a.LoadedComponents})


    /// Value set to None if the connection width could not be inferred.
    type ConnectionsWidth = Map<ConnectionId, int option>

    /// Documents user circuit error found during connection width inference
    type WidthInferError = {
        Msg : string
        ConnectionsAffected : ConnectionId list // A list of connection Ids.
    }


    /// Messages sent from draw block
    type JSDiagramMsg =
        | InitCanvas of CanvasState // Has to be dispatched only once.
        | SelectComponent of Component
        | UnselectComponent of unit
        | InferWidths of unit
        | SetHasUnsavedChanges of bool
