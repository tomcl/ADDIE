(*
This module draws schematics component symbols. Each symbol is associated with a unique Addie component.
*)

module Symbol
open CommonTypes
open DrawHelpers
open DrawModelType.SymbolT



/// --------- STATIC VARIABLES --------- ///
module Constants =
    [<Literal>]
    let gridSize = 30 
    let mergeSplitTextSize = "12px"
    let busSelectTextSize = "12px"
    let portTextSize = "12px"
    let portTextCharWidth = 7.
    let portTextWeight = "bold"
    let customPortSpacing = 40.
    let portPosEdgeGap = 0.7
    let gatePortPosEdgeGap = 0.3
    let legendVertOffset = 5.
    let legendLineSpacingInPixels = 16.

    /// How large are component labels
    let labelFontSizeInPixels:float = 16 // otehr parameters scale correctly with this

    /// Due to a bug in TextMetrics we are restricted to monospace font, bold or normal, or helvetica, if we want
    /// accurate width
    let componentLabelStyle: Text = 
        {defaultText with 
            TextAnchor = "start"; 
            FontSize = $"%.0f{labelFontSizeInPixels}px"; 
            FontFamily = "helvetica"; 
            FontWeight="600"}

    /// Style used by bus select bit legends
    let busSelectStyle: Text = 
        {defaultText with 
            TextAnchor = "start"; 
            FontSize = "12px"; 
            FontFamily = "helvetica"; 
            FontWeight="600"}

    /// Offset between label position and symbol. This is also used as a margin for the label bounding box.
    let componentLabelOffsetDistance: float = 7. // offset from symbol outline, otehr parameters scale correctly
    let thinComponentLabelOffsetDistance: float = 3.
    
    /// Height of label text - used to determine where to print labels
    let componentLabelHeight: float = labelFontSizeInPixels

    /// Small manual correction added to claculated position for placing labels.
    /// Used to make labels equidistant on all sides of symbol.
    let labelCorrection = {X= 0.; Y= 0.}
    
    
//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.

/// Returns the correct height and width of a transformed symbol
/// as the tuple (real H, real W).
/// Needed because H & W in Component do not chnage with rotation.
/// NB Pos in component = Pos in Symbol and DOES change with rotation!
let inline getCompRotatedHAndW (comp: Component) (transform: STransform) hScale vScale  =
    let hS,vS = (Option.defaultValue 1.0 hScale),(Option.defaultValue 1.0 vScale)
    match transform.Rotation with
    | Degree0 | Degree180 -> comp.H*vS, comp.W*hS
    | Degree90 | Degree270 -> comp.W*hS, comp.H*vS

/// Returns the correct height and width of a transformed symbol
/// as the tuple (real H, real W).
/// Needed because H & W in Component do not chnage with rotation.
/// NB Pos in component = Pos in Symbol and DOES change with rotation!
let inline getRotatedHAndW sym  = getCompRotatedHAndW sym.Component sym.STransform sym.HScale sym.VScale

/// returns the true centre of a component's symbol
let inline getRotatedCompCentre comp transform hScale vScale =
    // true component BB is (comp.X,comp.Y), h, w
    let h,w = getCompRotatedHAndW comp transform hScale vScale
    let centreX = comp.X + w / 2.
    let centreY = comp.Y + h / 2.
    {X=centreX;Y=centreY}

/// returns the true centre of a symbol, taking into account
/// its current rotation
let inline getRotatedSymbolCentre (symbol:Symbol) =
    getRotatedCompCentre symbol.Component symbol.STransform symbol.HScale symbol.VScale
/// Returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol.
/// Works with rotation. For a rotated symbol, TopLeft = Pos, and H,W swapped in getrotatedHAndW
let inline getSymbolBoundingBox (sym:Symbol): BoundingBox =
    let h,w = getRotatedHAndW sym
    {TopLeft = sym.Pos; H = float(h) ; W = float(w)}

type Symbol with
    member this.SymbolBoundingBox = getSymbolBoundingBox this

/// Returns all the bounding boxes of all components in the model
let getBoundingBoxes (symModel: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun sId (sym:Symbol) -> (getSymbolBoundingBox sym)) symModel.Symbols

/// Returns bounding box of a component based on component id
let inline getBoundingBox (symModel: Model) (compid: ComponentId ): BoundingBox = 
    let symb = Map.find compid symModel.Symbols
    getSymbolBoundingBox symb

/// Returns the bounding boxes of all symbol labels in the model
let getLabelBoundingBoxes (model: Model) : Map<ComponentId, BoundingBox> =
    model.Symbols
    |> Map.map (fun _ sym -> sym.LabelBoundingBox)

/// Returns the bounding box of the symbol associated with compId
let getLabelBoundingBox (model: Model) (compId: ComponentId) : BoundingBox =
    Map.find compId model.Symbols
    |> (fun sym -> sym.LabelBoundingBox)



//------------------------------------------------------------------//
//------------------------ Helper functions ------------------------//
//------------------------------------------------------------------//

let moveSymbol (offset:XYPos) (sym:Symbol) :Symbol =
    let newPos = sym.Pos + offset
    let comp' = {sym.Component with X = newPos.X; Y = newPos.Y}
    {sym with 
        Component = comp'
        Pos = newPos
        LabelBoundingBox = {sym.LabelBoundingBox with TopLeft = sym.LabelBoundingBox.TopLeft + offset}
    }

let moveSymbols  (offset: XYPos) (model:Model) =
    {model with
        Symbols = 
            model.Symbols
            |> Map.map (fun _ symbol -> moveSymbol offset symbol)
    }

let inline ioPortStr (PortId s) = s

let inline invertRotation (rot: RotationType) =
    match rot with
    | RotateClockwise -> RotateAntiClockwise
    | RotateAntiClockwise -> RotateClockwise



let inline combineRotation (r1:Rotation) (r2:Rotation) =
    let rot90 rot =
        match rot with
        | Degree0 -> Degree90
        | Degree90 -> Degree180
        | Degree180 -> Degree270
        | Degree270 -> Degree0
    let rot180 rot =
        match rot with 
        | Degree0 -> Degree180
        | Degree90 -> Degree270
        | Degree180 -> Degree0
        | Degree270 -> Degree90
    match r1 with
    | Degree0 -> r2
    | Degree90 -> rot90 r2
    | Degree180 -> rot180 r2
    | Degree270 -> (rot90 >> rot180) r2


    
let getSymbolColour compType clocked (theme:ThemeType) =
    match theme with
    | White | Light -> "#E8D0A9"//"lightgray"
    | Colourful ->
        match compType with
        |DiodeR -> "rgba(255,255,0,0.15)" //lightyellow: for combinational components
        |_ -> "#E8D0A9"  //dark orange: for IO
        //| _ -> "rgba(255,255,0,0.15)" //lightyellow: for combinational components



/// Modify port position maps to move an existing Lefthand port (index in the list) to the bottom edge
let movePortToBottom (portMaps: PortMaps) index =
    let leftPorts = portMaps.Order[Left]
    let portId = leftPorts |> List.item index //get id of sel

    let newBottomPorts = [portId]
    let newLeftPorts = portMaps.Order[Left] |> List.removeAt index
    let newPortOrder =
        portMaps.Order
        |> Map.add Bottom newBottomPorts
        |> Map.add Left newLeftPorts
    let newPortOrientation =
        portMaps.Orientation |> Map.add portId Bottom
    {Order=newPortOrder; Orientation=newPortOrientation}


/// Work out a label bounding box from symbol, return symbol with box added.
/// The box has a margin Constants. componentLabelOffsetDistance around the label text outline.
/// This function should be included at the end of any function that changes component 
/// or label position or orientation or shape.
let calcLabelBoundingBox (sym: Symbol) =
    let textStyle = Constants.componentLabelStyle
    let transform = sym.STransform
    let comp = sym.Component
    let labelRotation = 
        match transform.flipped with
        | true -> match transform.Rotation with
                     | Degree90 -> Degree270
                     | Degree270 -> Degree90
                     | _ -> transform.Rotation
        | false -> transform.Rotation
        |> combineRotation (Option.defaultValue Degree0 sym.LabelRotation)
    let h,w = getRotatedHAndW sym
    let centre = getRotatedSymbolCentre sym

    let margin = 
        match sym.Component.Type with
        | IOLabel  -> Constants.thinComponentLabelOffsetDistance
        | _ -> Constants.componentLabelOffsetDistance
    let labH = Constants.componentLabelHeight //height of label text
    //let labW = getMonospaceWidth textStyle.FontSize comp.Label
    let labW = getTextWidthInPixels(comp.Label,textStyle)// width of label text
    let boxTopLeft =
        match labelRotation with 
        |Degree90 |Degree270 -> sym.Pos + {X=w + 7.; Y=(h/8.)}
        |Degree0 | Degree180 -> sym.Pos + {X=(0.);Y=(-20.)}
    let box =
        match comp.Label, sym.LabelHasDefaultPos with
        | "", _ -> 
            {TopLeft=boxTopLeft; W = 0.; H = 0.}
        | _, true -> 
            {TopLeft=boxTopLeft; W = labW + 2.*margin; H = labH + 2.*margin}
        | _, false -> 
            sym.LabelBoundingBox
    {sym with LabelBoundingBox = box}


//------------------------------------------------------------------//
//------------------- Helper functions for titles ------------------//
//------------------------------------------------------------------//

///Insert titles compatible with greater than 1 buswidth
let busTitleAndBits (t:string) (n:int) : string =  
    match n with
    | 1 -> 
        t
    | _ when t = "" && n > 1 -> 
        $"{t}({n-1}:{0})"
    | _ when n > 1 -> 
        $"{t}.({n-1}:{0})"
    | _ -> 
        failwith "non positive bus width"


let nBitsGateTitle (gateType:string) (n:int) : string =
    match n with
    |1 -> gateType
    |_ -> (string n) + "-bit " + gateType 

///Insert titles for bus select
/// used once 
let busSelectTitle (wob:int) (lsb:int) : string = 
    match wob with
    | 1 -> $"{lsb}"
    | _ when wob > 1 -> $"({wob+lsb-1}:{lsb})"
    | _ -> failwith "non positive bus width in bustitle"

///Decodes the component type into component labels
let getPrefix (compType:ComponentType) = 
    match compType with
    | Resistor _ -> "R"
    | Inductor _ -> "L"
    | Capacitor _ -> "C"
    | DiodeL |DiodeR -> "D"
    | Opamp -> "OP"
    | VoltageSource _ -> "VS"
    | CurrentSource _ -> "CS"
    | Ground -> "G"
    |_  -> ""


// Input and Output names of the ports depending on their ComponentType
let portNames (componentType:ComponentType)  = //(input port names, output port names)
    ([]@[])
   

/// Genererates a list of ports:
let portLists numOfPorts hostID =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                HostId = hostID
            }])


//-----------------------Skeleton Message type for symbols---------------------//



let customToLength (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

/// get the max length (in pixels) of any of the Text strings in a list
/// Hack - for now assume text char width is constant
let customStringToLength (lst: string list) =
    let labelLengths = List.map String.length lst
    if List.isEmpty labelLengths then 0
    else List.max labelLengths
    |> float
    |> (*) Constants.portTextCharWidth

let addPortToMaps (edge: Edge) (portMaps: PortMaps) (portId: string) =
    {
        Order = portMaps.Order |> Map.add edge (portMaps.Order[edge] @ [portId])
        Orientation = portMaps.Orientation |> Map.add portId edge
    }

let deletePortFromMaps (port: string) (portMaps: PortMaps) =
    let deletePort (ports: string list) = List.filter ((<>) port) ports
    {
        Order = Map.map (fun edge pL -> deletePort pL) portMaps.Order
        Orientation = Map.remove port portMaps.Orientation
    }

/// work out the initial (default) port placing for a componenent.
/// also used for legacy circuits loaded without port layoiut info.
let initPortOrientation (comp: Component) =

    match comp.Type with
    | Ground -> 
        let order = Map [ (Top, [comp.IOPorts[0].Id]); (Left,[]);(Bottom,[]);(Right,[]);]
        let orientation = Map [ (comp.IOPorts[0].Id,Top);]
        {Order = order; Orientation = orientation}
    | VoltageSource _ | CurrentSource _ ->
        let order = Map [ (Top, [comp.IOPorts[0].Id]); (Bottom, [comp.IOPorts[1].Id]);(Left,[]);(Right,[]); ]
        let orientation = Map [ (comp.IOPorts[0].Id,Top); (comp.IOPorts[1].Id,Bottom) ]
        {Order = order; Orientation = orientation}
    | Opamp ->
        let order = Map [ (Left,[comp.IOPorts[0].Id;comp.IOPorts[1].Id]); (Top, []); (Bottom, []);(Right,[comp.IOPorts[2].Id]); ]
        let orientation = Map [ (comp.IOPorts[0].Id,Left); (comp.IOPorts[1].Id,Left);(comp.IOPorts[2].Id,Right); ]
        {Order = order; Orientation = orientation}
    |_ -> //TODO: Analyse this
        let order = Map [ (Left, [comp.IOPorts[0].Id]); (Right, [comp.IOPorts[1].Id]);(Top,[]);(Bottom,[]); ]
        let orientation = Map [ (comp.IOPorts[0].Id,Left); (comp.IOPorts[1].Id,Right) ]
        {Order = order; Orientation = orientation}
        


/// Needed because the I/Os of a custom component can be changed on anotehr sheet.
/// When the component is reloaded its port maps will be inconsistent.
/// This function keeps existing layout, and adds new I/Os or deletes old ones.
let makeMapsConsistent (portIdMap: Map<string,string>) (sym: Symbol) =
    let newPortIds = Set (portIdMap |> Map.keys)
    let currentPortIds = Set (sym.PortMaps.Orientation |> Map.keys)
    let addedPortIds = newPortIds - currentPortIds
    let deletedPortIds = currentPortIds - newPortIds
    let maps = sym.PortMaps
    (maps, addedPortIds) 
    ||> Set.fold (fun maps port -> 
                    let edgeToAddTo = // default add new outputs on right, inputs on left
                        match List.exists (fun (p:Port) -> p.Id = port) sym.Component.IOPorts with
                        | true -> Left
                        | false -> Right
                    addPortToMaps edgeToAddTo maps port)
    |> (fun maps -> maps, deletedPortIds)
    ||> Set.fold (fun maps port -> deletePortFromMaps port maps )

/// adjust symbol (and component) dimensions based on current ports and labels of a custom component.
/// leaves other symbols unchanged
let autoScaleHAndW (sym:Symbol) : Symbol =
    let comp = sym.Component
    {sym with Component = {comp with X = sym.Pos.X; Y = sym.Pos.Y}}
    |> calcLabelBoundingBox

/// return (num ports, height, width)
let getComponentProperties (compType:ComponentType) (label: string)=
    // match statement for each component type. the output is a 3-tuple that is used as an input to makecomponent (see below)
    // 3-tuple of the form ( number of ports, , Height, Width)
    let gS = float Constants.gridSize
    match compType with
    | DiodeL |DiodeR  -> ( 2 , 1.0*gS ,  1.0*gS) 
    | Capacitor _ -> ( 2 , 1.0*gS ,  2.0*gS)
    | Resistor _ | Inductor _ -> ( 2 , gS ,  3.0*gS)
    | VoltageSource _| CurrentSource _ -> (2, 2.*gS,2.*gS )
    | Ground -> (1, gS, gS)
    | Opamp -> (3, 2.*gS, 2.*gS)
    | IOLabel  ->(  2 , gS/2. ,  gS)
    | IO -> ( 1 , gS ,  2.*gS)
    
/// make a completely new component
let makeComponent (pos: XYPos) (compType: ComponentType) (id:string) (label:string) : Component =
    let defaultSTransform = {Rotation = Degree0; flipped = false}
    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let makeComponent' (n, h, w) label : Component=
        let ioPorts = portLists n id
        {
            Id = id 
            Type = compType
            Label = label 
            IOPorts = ioPorts
            X  = pos.X - float w / 2.0
            Y = pos.Y - float h / 2.0
            H = float h 
            W = float w
            SymbolInfo = Some { 
                LabelBoundingBox = None
                LabelRotation = None
                STransform=defaultSTransform; 
                PortOrder = Map.empty; 
                PortOrientation=Map.empty
                HScale = None
                VScale = None}
        }
    let props = getComponentProperties compType label           
    makeComponent' props label



/// Function to generate a new symbol
let createNewSymbol (ldcs: LoadedComponent list) (pos: XYPos) (comptype: ComponentType) (label:string) (theme:ThemeType) =
    let id = JSHelpers.uuid ()
    let style = Constants.componentLabelStyle
    let comp = makeComponent pos comptype id label
    let transform = 
        match comptype with
        |VoltageSource _ |CurrentSource _ -> {Rotation= Degree90; flipped= false}
        |_ -> {Rotation= Degree0; flipped= false}

    { 
      Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
      LabelBoundingBox = {TopLeft=pos; W=0.;H=0.} // dummy, will be replaced
      LabelHasDefaultPos = true
      LabelRotation = None
      Appearance =
          {
            HighlightLabel = false
            ShowPorts = ShowNone
            Colour = getSymbolColour comptype false theme
            Opacity = 1.0
          }
      Id = ComponentId id
      Component = comp
      Moving = false
      PortMaps = initPortOrientation comp
      STransform = transform
      MovingPort = None
      IsLinear = true
      MovingPortTarget = None
      HScale = None
      VScale = None
    }
    |> autoScaleHAndW
    |> calcLabelBoundingBox

// Function to add ports to port model     
let addToPortModel (model: Model) (sym: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    (model.Ports, sym.Component.IOPorts) ||> List.fold addOnePort
    
//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

/// hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
let inline getPortPosEdgeGap (ct: ComponentType) =
    //match ct with
    //| MergeWires | SplitWire _  -> 0.25
    //| IsBinaryGate | Mux2 -> Constants.gatePortPosEdgeGap
    //| _ -> 
    Constants.portPosEdgeGap

///Given a symbol and a Port, it returns the orientation of the port
let inline getSymbolPortOrientation (sym: Symbol) (port: Port): Edge =
    let portId = port.Id
    sym.PortMaps.Orientation[portId]


/// Returns the xy offset of a side relative to the symbol topleft
let inline getPortBaseOffset (sym: Symbol) (side: Edge): XYPos=
    let h,w = getRotatedHAndW sym
    match side with 
    | Right -> {X = w; Y = 0.0}
    | Left -> {X = 0.0; Y = 0.0}
    | Top -> {X = 0.0; Y = 0.0}
    | Bottom -> {X = 0.0; Y = h}


///Given a symbol and a port, it returns the offset of the port from the top left corner of the symbol
let getPortPos (sym: Symbol) (port: Port) : XYPos =
    //get ports on the same edge first
    let side = getSymbolPortOrientation sym port
    let ports = sym.PortMaps.Order[side] //list of ports on the same side as port
    let numberOnSide = List.length ports
    let index = ( List.findIndex (fun (p:string)  -> p = port.Id) ports )
    let index' = float(index)
    let gap = getPortPosEdgeGap sym.Component.Type 
    let topBottomGap = gap + 0.3 // extra space for clk symbol
    let baseOffset = getPortBaseOffset sym side  //offset of the side component is on
    let baseOffset' = baseOffset
    let portDimension = float ports.Length - 1.0
    //printfn "symbol %A portDimension %f" sym.Component.Type portDimension
    let h,w = getRotatedHAndW sym
    match side with
    | Left ->
        let yOffset = float h * ( index' + gap )/(portDimension + 2.0*gap)
        baseOffset' + {X = 0.0; Y = yOffset }
    | Right -> 
        let yOffset = float h * (portDimension - index' + gap )/(portDimension + 2.0*gap)
        baseOffset' + {X = 0.0; Y = yOffset }
    | Bottom -> 
        let xOffset = float  w * (index' + topBottomGap)/(portDimension + 2.0*topBottomGap)
        baseOffset' + {X = xOffset; Y = 0.0 }
    | Top ->
        let xOffset = float w * (portDimension - index' + topBottomGap)/(portDimension + 2.0*topBottomGap)
        baseOffset' + {X = xOffset; Y = 0.0 }

/// Gives the port positions to the render function, it gives the moving port pos where the mouse is, if there is a moving port
let inline getPortPosToRender (sym: Symbol) (port: Port) : XYPos =
    match sym.MovingPort with
    | Some movingPort when port.Id = movingPort.PortId -> movingPort.CurrPos - sym.Pos
    | _ -> 
        //printfn "symbol %A portDimension %A" sym.Component.Type (getPortPos sym port)
        getPortPos sym port

let inline getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols) port

//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
/// Returns the center coordinates of a Symbol
let getSymbolPos (symbolModel: Model) compId = //makes sense or should we have getSymbol?
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: Model) : (ComponentId list) =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst


/// Returns the port object associated with a given portId
let inline getPort (symModel: Model) (portId: string) =
    symModel.Ports[portId]

let inline getSymbol (model: Model) (portId: string) =
    let port = getPort model portId
    model.Symbols[ComponentId port.HostId]

let inline getCompId (model: Model) (portId: string) =
    let symbol = getSymbol model portId
    symbol.Id

/// Returns the string of a PortId
let inline getPortIdStr (portId: PortId) = 
    match portId with
    | Id (PortId id) -> id
    
let inline getIOPortIdStr (portId: IOPortId) = 
    match portId with
    | PortId s -> s

/// returns what side of the symbol the port is on
let inline getPortOrientation (model: Model)  (portId: PortId) : Edge =
    let portIdStr = getPortIdStr portId
    let port = model.Ports[portIdStr]
    let sId = ComponentId port.HostId
    model.Symbols[sId].PortMaps.Orientation[portIdStr]

let inline getIOPortOrientation (model: Model) (portId: IOPortId): Edge =
    getPortOrientation model (Id portId)

/// Returns the location of a given portId, with good efficiency
let getPortLocation (defPos: XYPos option) (model: Model) (portId : string) : XYPos=
    let portOpt = Map.tryFind portId model.Ports
    let symbolIdOpt = 
        portOpt
        |> Option.map (fun port ->  ComponentId port.HostId)
    let symOpt = 
        symbolIdOpt
        |> Option.bind (fun symbolId -> Map.tryFind symbolId model.Symbols)
    match defPos, symOpt, portOpt with
    | _, Some sym, Some port -> getPortPos sym port + sym.Pos
    | Some pos, _, _ ->
        printfn $"Can't find port or symbol: Port='{portOpt}', Symbol='{symOpt}"
        pos       
    | _ -> failwithf $"Can't find port or symbol: Port='{portOpt}', Symbol='{symOpt}"

/// Returns the location of an input port based on their portId
let inline getIOPortLocation defPos (model:Model) (portId: IOPortId)  = 
    let id = getPortIdStr (Id portId)
    getPortLocation defPos model id

///Returns the port positions of the specified symbols in model
let getPortsLocationMap (model: Model) (symbols: Symbol list)  = 
    let getSymbolInputPortsLoc sym =
        sym.Component.IOPorts 
        |> List.map (fun port -> (port.Id, (getPortPos sym port) + (sym.Pos)))
        
    symbols
    |> List.collect getSymbolInputPortsLoc
    |> Map.ofList


/// Returns all the port locations of the given components   
let getPortLocations (model: Model) (symbolIds: ComponentId list) = 
    let symbols = 
        model.Symbols 
        |> Map.filter (fun symbolId _  -> List.contains symbolId symbolIds)
        |> Map.toList
        |> List.map snd
        
    getPortsLocationMap model symbols 

/// Returns the locations of two given ports based on their portId
let inline getTwoPortLocations (model: Model) (portId1: IOPortId ) (portId2: IOPortId) =
    (getIOPortLocation None model portId1, getIOPortLocation None model portId2)