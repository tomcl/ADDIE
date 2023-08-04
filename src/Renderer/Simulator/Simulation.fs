module Simulation

open CommonTypes
open MathJsHelpers
open System
open CanvasStateAnalyser


//////////////////  SIMULATION CONSTANTS  ////////////////
module Constants =
    [<Literal>]
    let diodeConstant = 0.7
    let roundingDecimalPoints = 6
    let startACFreq = 0. //(exponent -> here: 10^0)
    let endACFreq = 7. //(exponent -> here: 10^7)
    let ACFreqStepsPerDecade = 20.
    let I_S = 3.35*(10.0**(-9.0))//(10.0**(-15.0))
    let V_t =  0.04942 //0.04621 //0.025875 
    let convergence_epsilon = (10.0**(-9.))
    let newtonRaphsonMaxIterations = 40

//////////////////  SIMULATION HELPERS   /////////////////

let combineGrounds (comps,conns) =
    let allGrounds = comps |> List.filter (fun c->c.Type = Ground)
    match List.isEmpty allGrounds with
    |true -> (comps,conns)
    |false -> 
        let mainGround,mainPort = allGrounds[0], allGrounds[0].IOPorts[0]
        let otherGrounds = List.removeAt 0 allGrounds
        let otherPortsIds = otherGrounds |> List.map (fun c -> c.IOPorts[0].Id)

        let conns' =
            conns
            |> List.map (fun conn->
                if List.exists (fun i -> i=conn.Source.Id) otherPortsIds then
                    {conn with Source = mainPort}
                elif List.exists (fun i -> i=conn.Target.Id) otherPortsIds then
                    {conn with Target = mainPort}
                else
                    conn        
            )

        let comps' = comps |> List.filter (fun c-> not (List.exists (fun (ground:Component) -> ground.Id = c.Id) otherGrounds))

        (comps',conns')

/// Transforms locally the canvas state to replace all
/// Inductors by 0-Volts DC Sources (short circuit) for
/// the MNA algorithm to work 
let shortInductorsForDC (comps,conns) =
    
    // necessary for current orientation
    let updateSymbolInfo siOpt =
        let rotateRight r = match r with |Degree90->Degree0|Degree270->Degree180|Degree180->Degree0|Degree0->Degree180 
        match siOpt with
        |Some si ->
            Some {si with STransform={si.STransform with Rotation=(rotateRight si.STransform.Rotation)}}
        |None -> None
    let comps' =
        comps
        |> List.map (fun c->
            match c.Type with
            |Inductor _ -> {c with Type = VoltageSource (DC 0.);SymbolInfo = (updateSymbolInfo c.SymbolInfo)}
            |_ -> c
        )
    (comps',conns)

let findAllVoltageSources comps =
    comps |> List.filter (fun c->match c.Type with |VoltageSource _ ->true |_->false)
    
let findAllInductors comps =
    comps |> List.filter (fun c->match c.Type with |Inductor _ ->true |_->false)

let findAllDiodes comps =
    comps |> List.filter (fun c-> c.Type = DiodeL)
   
let findCompFromId comps id =
    comps |> List.find (fun (c:Component)->c.Id=id)
            
let findInputAtTime vs t =
        match vs with
        |None -> failwithf "No Voltage Source present in the circuit"
        |Some v ->
            match v.Type with
            |VoltageSource (DC x) -> x
            |VoltageSource (Sine (a,dc,f,p)) ->
                a*sin(2.*System.Math.PI*f*t+p)+dc
            |VoltageSource (Pulse (v1,v2,f))->
                let md = t % f
                if md < f/2. then v1
                else v2
            |_ -> failwithf "Impossible"


/// Converts the input Voltage Source into a (DC 1) source so that the ratio 
/// (Output_Node/Input_Source) is correct. Other sources are converted to (DC 0)
let convertACInputToDC1VS (comps,conns:Connection list) inputSource =
    let inputComp = findCompFromId comps inputSource
    let updatedComp = {inputComp with Type = VoltageSource (DC 1)}
    comps
    |> List.map (fun c->
        if c.Id = inputSource
            then 
                updatedComp
        else 
            match c.Type with
            |VoltageSource _ -> {c with Type = VoltageSource (DC 0)}
            |_ -> c
        ),conns



let findVd (nodeList:(Component*int option) list list) compId (prevSolution:float array)  = 
    let nodePair =
        nodeList
        |> List.mapi (fun i localNode -> 
            localNode |> List.collect (fun (c,pNo)->
                match c.Id=compId with
                |true -> [(i,pNo)]
                |false -> []
            )
        )
        |> List.collect id
    //printfn "nodepair is: %A" nodePair
    //printfn "prev solution is: %A" prevSolution
    match nodePair with
    |[(n1,p1);(n2,p2)] -> 
        if p1 = Some 0 then
            if n1 = 0 then
                (-prevSolution[n2-1])
            else if n2 = 0 then
                prevSolution[n1-1]
            else
                prevSolution[n1-1]-prevSolution[n2-1]
        else if p2 = Some 0 then
            if n1 = 0 then
                prevSolution[n2-1]
            else if n2 = 0 then
                (-prevSolution[n1-1])
            else
                prevSolution[n2-1]-prevSolution[n1-1]
        else 
            0.0
    |_ -> 0.0


let getDiodeValues vd =
    //printfn "vd is %f" vd
    let geq = {Re= Constants.I_S*(exp(vd/Constants.V_t))/Constants.V_t; Im=0.}
    let ido = {Re= Constants.I_S*(exp(vd/Constants.V_t)-1.); Im=0.}
    let ieq = ido - geq*vd
    //printfn "diode values: %A" [ido;geq;ieq]
    ido,geq,ieq


/// Calculates the elements of the vector B of MNA
/// which is of the form:
/// [I1,I2,...,In,Va,Vb,...,Vm,0o] 
/// n -> number of nodes, m number of Voltage Sources, o number of opamps
let calcVectorBElement row comps (nodeToCompsList:(Component*int option) list list) (prevSolution:float array) =
    let nodesNo = List.length nodeToCompsList
    
    let findNodeTotalCurrent currentNode = 
        ({Re=0.;Im=0.}, currentNode) ||> List.fold (fun s (comp,no) ->
            //printfn "ROW= %i, comp= %A, no= %A" row comp no
            match (comp.Type,no) with
            |CurrentSource (v,_),Some 0 -> s+{Re=v;Im=0.}
            |CurrentSource (v,_), Some 1 -> s-{Re=v;Im=0.}
            |DiodeR, _ -> 
                let vd = findVd nodeToCompsList comp.Id prevSolution
                let _,_,ieq = getDiodeValues vd  
                match no with
                |Some 0 -> s-ieq  //lhs port
                |_ -> s+ieq       //rhs port
            |_ -> s
        )

    let allVoltageSources = findAllVoltageSources comps
    
    let vsNo = List.length allVoltageSources

    if row < nodesNo then   //current rows
        let currentNode = nodeToCompsList[row]
        findNodeTotalCurrent currentNode
    else if row < (nodesNo + vsNo) then  //voltage source rows
        allVoltageSources[row-nodesNo]
        |> (fun c -> 
            match c.Type with
            |VoltageSource (DC v) -> {Re=v;Im=0.}
            |VoltageSource (Sine (_,dc,_,_)) -> {Re=dc;Im=0.}
            |_ -> failwithf "Impossible"
        )
    else    //opamp rows
        {Re=0.;Im=0.}






/// Calculates the value of the MNA matrix at the specified
/// row and column (Count starts from 1)
/// Matrix consist of submatrices
/// More info on github
/// A = [G B]
///     [C D]
let calcMatrixElementValue row col (nodeToCompsList:(Component*int option) list list) comps omega prevSolution  = 
    let findMatrixGCompValue (comp,no) omega =
        match comp.Type with
        |Resistor (v,_) -> {Re= (1./v)*1.; Im=0.}
        |Inductor (v,_) -> {Re = 0.; Im= -(1./(v*omega)*1.)}
        |Capacitor (v,_) -> {Re = 0.; Im= (v*omega)*1.}
        |DiodeR -> 
            let vd = findVd nodeToCompsList comp.Id prevSolution
            {Re= Constants.I_S*(exp(vd/Constants.V_t))/Constants.V_t; Im=0.}
        |CurrentSource _ |VoltageSource _ |Opamp  -> {Re = 0.0; Im=0.0}
        |_ -> failwithf "Not Implemented yet"
    

    let findMatrixBVoltageValue (comp,no) (vsAndOpamps:Component list) col nodesNo =
        match comp.Type with
        |VoltageSource (_) |Opamp ->
            match comp.Id = vsAndOpamps[col-nodesNo].Id with
            |true ->
                match (comp.Type,no) with
                |VoltageSource (_),Some 1 -> {Re = -1.0; Im=0.0}
                |VoltageSource (_), Some 0 -> {Re = 1.0; Im=0.0}
                |Opamp, Some 2 -> {Re = 1.0; Im=0.0}
                |Opamp, _ -> {Re = 0.0; Im=0.0}
                |_ -> failwithf "Unable to identify port of Voltage Source"
            |false -> {Re = 0.0; Im=0.0}
        |_ -> {Re = 0.0; Im=0.0}

    let findMatrixCVoltageValue (comp,no) (vsAndOpamps:Component list) col nodesNo =
        match comp.Type with
        |VoltageSource (_) |Opamp ->
            match comp.Id = vsAndOpamps[col-nodesNo].Id with
            |true ->
                match (comp.Type,no) with
                |VoltageSource (_),Some 1 -> {Re = -1.0; Im=0.0}
                |VoltageSource (_), Some 0 -> {Re = 1.0; Im=0.0}
                |Opamp, Some 0 -> {Re = 1.0; Im=0.0}
                |Opamp, Some 1 -> {Re = -1.0; Im=0.0}
                |Opamp, _ -> {Re = 0.0; Im=0.0}
                |_ -> failwithf "Unable to identify port of Voltage Source"
            |false -> {Re = 0.0; Im=0.0}
        |_ -> {Re = 0.0; Im=0.0}
                
    let nodesNo = List.length nodeToCompsList
    
    let allVoltageSources = findAllVoltageSources comps
    
    let allOpamps = 
        nodeToCompsList
        |> List.removeAt 0
        |> List.collect (fun nodeComps ->
            nodeComps
            |> List.collect (fun (comp,_) ->
                match comp.Type with
                |Opamp -> [comp]
                |_ -> []
            )
        )
        |> List.distinctBy (fun c->c.Id)

    let vsNo = List.length allVoltageSources

    let vsAndOpamps = allVoltageSources @ allOpamps


    if (row < nodesNo && col < nodesNo) then
    // conductance matrix
        if row=col then
            ({Re=0.;Im=0.}, nodeToCompsList[row]) 
            ||> List.fold(fun s c ->
                s + (findMatrixGCompValue c omega)
            )

        else
            ({Re=0.;Im=0.}, findComponentsBetweenNodes (row,col) nodeToCompsList)
            ||> List.fold(fun s c ->
                s - (findMatrixGCompValue c omega)
            )
    else 
    // extra elements for modified nodal analysis (vs / opamp)
        if (row >= nodesNo && col >= nodesNo) then
            // matrix D is always 0
            {Re=0.;Im=0.}
        else
            if row < nodesNo then
                ({Re=0.;Im=0.}, nodeToCompsList[row])
                ||> List.fold(fun s c ->
                    s + (findMatrixBVoltageValue c vsAndOpamps col nodesNo)
                )            
            else
                ({Re=0.;Im=0.}, nodeToCompsList[col])
                ||> List.fold(fun s c ->
                    s + (findMatrixCVoltageValue c vsAndOpamps row nodesNo)
                )       



/// Finds resistor, vs and opamp currents
/// Current direction in vs will always be from + to -
/// In opamps: output to input(V+/V-)
/// In resistors: left2right or top2bottom
let findComponentCurrents results nodesNo nodeLst comps conns =
    let allVoltageSources = findAllVoltageSources comps
    
    let allOpamps = 
        nodeLst
        |> List.removeAt 0
        |> List.collect (fun nodeComps ->
            nodeComps
            |> List.collect (fun (comp,_) ->
                match comp.Type with
                |Opamp -> [comp]
                |_ -> []
            )
        )
        |> List.distinctBy (fun c->c.Id)

    let allResistors = 
        nodeLst
        |> List.removeAt 0
        |> List.collect (fun nodeComps ->
            nodeComps
            |> List.collect (fun (comp,_) ->
                match comp.Type with
                |Resistor _ -> [comp]
                |_ -> []
            )
        )
        |> List.distinctBy (fun c->c.Id)
    
    let vsAndOpampCurrents =
        allVoltageSources@allOpamps
        |> List.mapi (fun i comp ->
            let current = Array.tryItem (i+nodesNo) results
            match current with
            |Some curr -> 
                match comp.Type,comp.SymbolInfo with
                |Opamp,Some x when x.STransform.Rotation = Degree270||x.STransform.Rotation = Degree0 -> (ComponentId comp.Id, -curr)
                |VoltageSource _,Some x when x.STransform.Rotation = Degree270||x.STransform.Rotation = Degree0 -> (ComponentId comp.Id, -curr)
                |_-> (ComponentId comp.Id, curr)
            |None -> failwithf "Attempting to find current that doesn't exist in the result vector"
        )

    let topLeftToBottomRight n1 n2 (comp: Component) =
        let conns1 = findConnectionsOnNode nodeLst n1 conns
        let conns2 = findConnectionsOnNode nodeLst n2 conns
        let portId1 = conns1 |> List.collect (fun conn -> 
            if conn.Source.Id = comp.IOPorts[0].Id 
            || conn.Source.Id = comp.IOPorts[1].Id 
                then [conn.Source.Id]
                else if conn.Target.Id = comp.IOPorts[0].Id 
                || conn.Target.Id = comp.IOPorts[1].Id 
                then [conn.Target.Id]
                else []
            )
        let portId2 = conns2 |> List.collect (fun conn -> 
            if conn.Source.Id = comp.IOPorts[0].Id 
            || conn.Source.Id = comp.IOPorts[1].Id 
                then [conn.Source.Id]
                else if conn.Target.Id = comp.IOPorts[0].Id 
                || conn.Target.Id = comp.IOPorts[1].Id 
                then [conn.Target.Id]
                else []
            )

        let edge1,edge2 = 
            match comp.SymbolInfo with
            |None -> Top,Bottom
            |Some info ->
                info.PortOrientation[portId1[0]], info.PortOrientation[portId2[0]]

        match edge1,edge2 with
        |Top,Bottom |Left,Right -> n2,n1
        |Bottom,Top |Right,Left -> n1,n2
        |_ -> failwithf "Cannot identify node orientation relative to symbol"


    let resistorCurrents= 
        allResistors
        |> List.map (fun c-> findNodesOfComp nodeLst c.Id)
        |> List.mapi (fun i pair->
            match pair with
            |Some (n1,n2) ->
                let n1',n2' = topLeftToBottomRight n1 n2 allResistors[i]
                let v1 = if n1'=0 then 0. else results[n1'-1]
                let v2 = if n2'=0 then 0. else results[n2'-1]
                let resistance = 
                    match allResistors[i].Type with 
                    |Resistor (v,_) -> v 
                    |_ -> failwithf "Attempting to find Resistance of a non-Resistor comp"
                (ComponentId allResistors[i].Id,(v2-v1)/resistance)
            |None -> (ComponentId allResistors[i].Id,0.)
        )
        |> List.map (fun (r,v)->(r, System.Math.Round (v,Constants.roundingDecimalPoints)))
        
    vsAndOpampCurrents
    |> List.append resistorCurrents
    |> Map.ofList


///////// MAIN SIMULATION FUNCTIONS //////////


/// Performs MNA on the current canvas state. 
/// Returns: (i) result of MNA (node Voltages, vs/opamp currents)
/// (ii) component Currents, (iii) nodeToCompsList
let rec modifiedNodalAnalysisDC (comps,conns) cachedDiodeModes =
    
    if List.exists (fun c -> c.Type = DiodeR) comps then
        newtonRaphson (comps,conns)
    else

    // transform canvas state for DC Analysis  
    let comps',conns',localDiodeModes= 
        (comps,conns)
        |> combineGrounds
        |> shortInductorsForDC 
        |> (fun cs -> transformAllDiodes cs cachedDiodeModes)

    
    /////// required information ////////
    
    let nodeLst = createNodetoCompsList (comps',conns')
    
    let vs = comps' |> List.filter (fun c-> match c.Type with |VoltageSource _ -> true |_ -> false) |> List.length
    let opamps = comps' |> List.filter (fun c-> c.Type=Opamp) |> List.length

    let n = List.length nodeLst - 1 + vs + opamps
    
    
    let arr = Array.create n 0.0
    let matrix = Array.create n arr


    ////////// matrix creation ///////////
    

    let vecB =
        Array.create n {Re=0.0;Im=0.0}
        |> Array.mapi (fun i v -> (calcVectorBElement (i+1) comps' nodeLst [||]))
        |> Array.map (fun c -> c.Re)
    
    let flattenedMatrix =
        matrix
        |> Array.mapi (fun i top ->
            top
            |> Array.mapi (fun j v ->
                calcMatrixElementValue (i+1) (j+1) nodeLst comps' 0. [||]
            )
        )
        |> Array.collect (id)
     

    ////////// solve ///////////

    let mul = safeSolveMatrixVec flattenedMatrix vecB (List.length nodeLst - 1)
    match mul with
    |Some res -> 
        let result = res |> Array.map (fun x->System.Math.Round (x,Constants.roundingDecimalPoints))
        let componentCurrents = findComponentCurrents result (List.length nodeLst-1) nodeLst comps' conns'
        result, componentCurrents, nodeLst, localDiodeModes 

    |None -> Array.empty, Map.empty, nodeLst, []


/// This function takes place before modified nodal analysis
/// and transforms all diodes to either 0.7 V Voltage Sources
/// or 0 A current sources. It explores all posible combinations
/// (conducting, non-conducting mode) accordiong to the number of diodes 
/// and returns the combination that satisfies all the conditions
and transformAllDiodes (comps,conns) cachedDiodeModes : Component list * Connection list * bool list = 
    
    /// transforms CanvasState components according to the selected modes
    let runTransformation (diodeModeMap:Map<string,bool>) comps =
        comps
        |> List.map (fun c -> 
            if c.Type = DiodeL then 
                match diodeModeMap[c.Id] with
                |true -> {c with Type = (VoltageSource (DC Constants.diodeConstant))}
                |false -> {c with Type=(CurrentSource (0.,"0"))} 
            else c)
    
    /// Finds the next diode modes to be checked
    /// Starts from all true -> conducting
    /// Ends with all false -> non-conducting
    /// process: bool list -> string -> int -> -1 -> string -> bool list
    let nextModes (diodeModes:bool list) =
        let diodesNo = List.length diodeModes

        let asStr = ("",diodeModes) ||> List.fold (fun s v -> if v then s+"1" else s+"0")
        let asInt = Convert.ToInt32(asStr, 2);
        if asInt = 0 then //initial modes
            diodeModes |> List.mapi (fun i _ -> if i%2=1 then false else true)
        else 
            let nextAsStr = Convert.ToString(asInt-1, 2);
            let extraZeros = 
                if String.length nextAsStr = diodesNo then
                    ""
                else
                    Array.create (diodesNo - String.length nextAsStr) '0' |> Array.toSeq |> string
            let wholeStr = extraZeros + nextAsStr 
            wholeStr |> Seq.toList |> List.collect (fun ch -> if ch = '0' then [false] else if ch='1' then [true] else [])
        
    let checkSingleDiodeCondition comps' (res:float array) nodeLst diodeId mode =      
        match res with
        |[||]-> //simulation didn't produce any results -> conditions not met
            false
        |_->
            match mode with
            |true -> //assumed conducting mode
                let allVoltageSources = findAllVoltageSources comps'
                let indexOfDiode = allVoltageSources |> List.findIndex (fun c->c.Id=diodeId)
                let nodesNo = List.length nodeLst-1
                if res[nodesNo+indexOfDiode] >= 0. then        
                    true
                else
                    false
            |false -> //assumed non-conducting mode
                let pair = findNodesOfComp nodeLst diodeId
                match pair with
                |Some (i1,i2) ->
                    let i1',i2' = if List.exists (fun (c:Component,pNo) -> c.Id = diodeId && pNo = Some 1) nodeLst[i1] then i1,i2 else i2,i1  
                    let res1 = if i1' = 0 then 0. else res[i1'-1]
                    let res2 = if i2' = 0 then 0. else res[i2'-1]
                    if (res2 - res1) <= Constants.diodeConstant then
                        true
                    else 
                        false 
                |None -> false
        
    ///////////// FUNCTION BODY  //////////////

    let diodes = findAllDiodes comps |> List.map (fun c->c.Id)
    let diodesNo = List.length diodes
    match diodesNo <> 0 with
    |true ->

        // in diodeModes: false-> non-conducting , true -> conducting
        let mutable diodeModes = 
            match cachedDiodeModes with
            |[] -> Array.create diodesNo true |> Array.toList
            |_ -> cachedDiodeModes
        
        // used to store the modes once all conditions are satisfied
        let mutable correctDiodeModes = []
        
        // is true when all conditions are satisfied
        // used to stop the loop
        let mutable allConditions = false

        // used to force stop the loop in case no solution was found
        let mutable iter = pown 2 diodesNo
        

        while (not allConditions) do    
            
            let diodeModeMap = List.zip diodes diodeModes |> Map.ofList
            let comps' = runTransformation diodeModeMap comps
            let res,_,nodeLst,_ = modifiedNodalAnalysisDC (comps',conns) []
        
            // holds (per diode) whether the condition is satisfied
            let conditionList = 
                diodeModeMap 
                |> Map.map (fun dId mode -> (checkSingleDiodeCondition comps' res nodeLst dId mode))
                |> Map.values
                |> Seq.toList

            // update allColditions
            allConditions <- (true,conditionList) ||> List.fold (fun s v -> s&&v)
            iter <- iter - 1
            
            //force stop
            if iter = 0 then
                allConditions <- true

            // if all conditions are met, save the current diode modes to return them
            if allConditions then
                correctDiodeModes <- diodeModes
            
            // find next diode modes to be checked
            let next = nextModes diodeModes
            diodeModes <- next



        
        // extract updated CanvasState and return it
        let diodeModeMap = List.zip diodes correctDiodeModes |> Map.ofList
        let compsFinal = runTransformation diodeModeMap comps
        (compsFinal,conns,correctDiodeModes) 
    
    |false ->
        // if no diodes are present in the circuit -> do nothing
        (comps,conns,[])


/// implementation of newton-raphson with MNA
/// currenlty works for real diodes only (DiodeR)
and newtonRaphson (comps,conns) =
    let hasConverged (prev: float array ) (newSols: float array) =
        newSols
        |> Array.mapi (fun i v -> if abs(v-prev[i]) < Constants.convergence_epsilon then true else false)
        |> (fun bools -> 
            (true, bools) ||> Array.fold (fun s v -> s&&v))
            

    // transform to linearized diodes to obtain initial solution
    let linDiodeComps = comps |> List.map (fun c-> match c.Type with |DiodeR -> {c with Type = DiodeL} |_ -> c)
    let initialSols,_,_,_ = modifiedNodalAnalysisDC (linDiodeComps,conns) []
    
    
    // algorithm setup
    let mutable prevsols = initialSols
    let comps',conns'= 
        (comps,conns)
        |> combineGrounds
        |> shortInductorsForDC 
    
    let nodeLst = createNodetoCompsList (comps',conns')
    
    let vs = comps' |> List.filter (fun c-> match c.Type with |VoltageSource _ -> true |_ -> false) |> List.length
    let opamps = comps' |> List.filter (fun c-> c.Type=Opamp) |> List.length

    let n = List.length nodeLst - 1 + vs + opamps
    
    
    let arr = Array.create n 0.0
    let matrix = Array.create n arr
    
    let mutable max_iter = Constants.newtonRaphsonMaxIterations


    // newton raphson implementation
    while max_iter <> 0 do

        ////////// matrix creation for MNA ///////////

        let vecB =
            Array.create n {Re=0.0;Im=0.0}
            |> Array.mapi (fun i v -> (calcVectorBElement (i+1) comps' nodeLst prevsols))
            |> Array.map (fun c -> c.Re)
    
        let flattenedMatrix =
            matrix
            |> Array.mapi (fun i top ->
                top
                |> Array.mapi (fun j v ->
                    calcMatrixElementValue (i+1) (j+1) nodeLst comps' 0. prevsols
                )
            )
            |> Array.collect (id)

        let mul = safeSolveMatrixVec flattenedMatrix vecB (List.length nodeLst - 1)
        match mul with
        |Some res -> 
            if hasConverged res prevsols then
                max_iter <- 1 //exit loop
                prevsols <- res |> Array.map (fun x->System.Math.Round (x,Constants.roundingDecimalPoints))
            else 
                prevsols <- res
        |None ->
            max_iter <- 1  //force exit loop, keep previous solutions

        //printfn "Solution in iter %i is : %A" iter prevsols
        max_iter <- max_iter - 1
    
    let componentCurrents = findComponentCurrents prevsols (List.length nodeLst-1) nodeLst comps' conns'
    prevsols, componentCurrents, nodeLst, [] 


/// Returns magnitude and phase of specific frequency for the specified node 
let acAnalysis matrix nodeLst comps vecB wmega outputNode =    
    let flattenedMatrix =
        matrix
        |> Array.mapi (fun i top ->
            top
            |> Array.mapi (fun j v ->
                calcMatrixElementValue (i+1) (j+1) nodeLst comps wmega [||]
            )
        )
        |> Array.collect (id)
    let result = safeSolveMatrixVecComplex flattenedMatrix vecB (List.length nodeLst - 1)
    //printfn "result %A" result
    result[outputNode-1]        

     
/// Main frequency response function
/// Performs AC Analysis in the circuit for the pre-defined set
/// of frequencies and returns a list of ComplexP (output/input)
let frequencyResponse (comps,conns) inputSource outputNode  =

    let convertDiodeToConductingMode (comps,conns) =
        comps 
        |> List.map (fun c-> 
            if c.Type = DiodeL 
                then {c with Type = (VoltageSource (DC Constants.diodeConstant))} 
            else c),conns
     
    // transform canvas state for AC Analysis  
    let comps',conns' = 
        (comps,conns)
        |> combineGrounds
        |> (fun CS -> convertACInputToDC1VS CS inputSource)
        |> convertDiodeToConductingMode

    let nodeLst = createNodetoCompsList (comps',conns')

    let vs = 
        comps'
        |> List.filter (fun c-> match c.Type with |VoltageSource _ -> true |_ -> false)
        |> List.length

    let opamps = comps' |> List.filter (fun c-> c.Type=Opamp) |> List.length

    // setup matrix and vecB
    let n = List.length nodeLst - 1 + vs + opamps
    let arr = Array.create n 0.0
    let matrix = Array.create n arr    
    let vecB =
        Array.create n {Re=0.0;Im=0.0}
        |> Array.mapi (fun i v -> (calcVectorBElement (i+1) comps' nodeLst [||]))
    

    // Run Frequency response
    let frequencies = [Constants.startACFreq..(1./Constants.ACFreqStepsPerDecade)..Constants.endACFreq] |> List.map (fun x -> 10.**x)
    
    frequencies
    |> List.map (fun wmega->
        acAnalysis matrix nodeLst comps' vecB wmega outputNode
    )
    |> List.map complexCToP
    


/// helper function used for finding the thevenin parameters
let replaceCompWithCS csValue compId (comps,conns) =
    let comps' =
        comps
        |> List.map (fun (c:Component)->
            match c.Id with
            |cid when cid=compId -> {c with Type = CurrentSource (csValue,"0")}
            |_ -> c    
        )
    comps',conns

/// need to make sure that only 1 capacitor/inductor is present in the circuit 
/// Currently guaranteed by the error checking function
let transformForHFGain (comps,conns) =
    let comps' =
        comps
        |> List.map (fun c->
            match c.Type with
            |Inductor _ -> {c with Type = CurrentSource (0.,"0")}
            |Capacitor _ -> {c with Type = VoltageSource (DC 0.)}
            |_ -> c    
        )
    (comps',conns)
        

/// Performs time domain analysis on a circuit with no Reactive Components
let DCTimeAnalysis (comps,conns) inputNode outputNode =
    let vsIndex = comps |> List.tryFindIndex (fun c->match c.Type with |VoltageSource _ -> true |_ -> false)
    match vsIndex with
    |Some i ->
        let vs = comps[i]
        let f = match vs.Type with |VoltageSource (Sine (_,_,f,_)) -> f |_ -> 0.
        let dts = if f=0. then [0.0..(1./100.)..10.] else [0.0..(1./(100.*f))..5./f]

        let mutable prevDiodeModes = []
        let y =
            dts
            |> List.map (fun t->
                let vIn = findInputAtTime (Some vs) t 
                let comps' = List.updateAt i {vs with Type = VoltageSource (DC vIn)} comps
                let res,_,_,dm = modifiedNodalAnalysisDC (comps',conns) prevDiodeModes
                prevDiodeModes <- dm
                res[outputNode-1]
            )
        {TimeSteps=dts;Transient=[];SteadyState=y;Tau=0;Alpha=0;HFGain=0;DCGain=0}
        
    |_ -> failwithf "No voltage Source present in the circuit"


/// Helper function to find the thevenin parameters
/// Changes the specified component to a 1A and 2A Current Source
/// from which it obtains v1 and v2. Then finds parameters from the
/// line that goes through these two points
/// (same as finding thevenin with nodal analysis)
let findTheveninParams (comps,conns) compId node1 node2 =
        let result1,_,_,_ =
            (comps,conns)
            |> replaceCompWithCS 1. compId
            |> (fun cs -> modifiedNodalAnalysisDC cs [])
        
        let result2,_,_,_ =
            (comps,conns)
            |> replaceCompWithCS 2. compId
            |> (fun cs -> modifiedNodalAnalysisDC cs [])

        let v1 =
            if node1=0 then result1[node2-1]
            else if node2=0 then result1[node1-1]
            else result1[node2-1] - result1[node1-1]
        
        let v2 =
            if node1=0 then result2[node2-1]
            else if node2=0 then result2[node1-1]
            else result1[node2-1] - result2[node1-1]

        let rth = abs(v2-v1)
        let vth = if v2-v1 > 0 then (v1 - rth) else (v1 + rth)
        let ino = vth/rth
        {Resistance=rth;Voltage=vth;Current=ino}

/// Main transient analysis function
/// (i) finds tau from Thevenin Resistance
// (ii) finds DC/HF gain from circuit 
// (iii) takes input signal, finds A and returns t,ytr,yss
let transientAnalysis (comps,conns) inputSource inputNode outputNode =
    
    let comps',conns' = combineGrounds (comps,conns)
    
    let findTau param =
        let CL = comps' |> List.tryFind (fun c-> match c.Type with |Capacitor _ |Inductor _ -> true |_ ->false)
        match CL with
        |None -> failwithf "No Capacitor/Inductor present in the circuit"
        |Some cl ->
            let pair = findNodesOfComp (createNodetoCompsList (comps',conns')) cl.Id
            match pair with
            |Some (node1,node2) ->
                let thevParams = findTheveninParams (comps,conns) cl.Id  node1 node2
                match cl.Type with
                |Capacitor (c,_) -> thevParams.Resistance*c
                |Inductor (l,_) -> l/thevParams.Resistance
                |_ -> failwithf "No Capacitor/Inductor present in the circuit"
            |None -> 0.

    let findDCGain nodeX nodeY =
        let result,_,_,_ = modifiedNodalAnalysisDC (comps',conns') []
        if result[nodeX-1] <> 0
            then result[nodeY-1]/result[nodeX-1]
        else 0.
    
    let findHFGain nodeX nodeY =
        let comps',conns' = (comps',conns') |> transformForHFGain //replaceCLWithWire
        let result,_,_,_ = modifiedNodalAnalysisDC (comps',conns') []
        
        if result[nodeX-1] <> 0 //DC source or Sine with DC offset
            then result[nodeY-1]/result[nodeX-1]
        else result[nodeY-1]

    match List.exists (fun c->match c.Type with |Capacitor _ |Inductor _ -> true |_ -> false) comps' with
    |true -> //one reactive component
        let vs = comps' |> List.tryFind (fun c->match c.Type with |VoltageSource _ -> true |_->false)
        let dcGain = findDCGain inputNode outputNode
        
        // represent yss as a Voltage Source to find
        // its value at each time point
        let yssAsVS =
            match vs with
            |Some comp ->
                match comp.Type with
                |VoltageSource (DC v) ->
                    let res,_,_,_ = modifiedNodalAnalysisDC (comps',conns') []
                    {comp with Type = VoltageSource (DC res[outputNode-1])}
                |VoltageSource (Sine (a,o,f,p)) -> 
                    let comps'',_ = convertACInputToDC1VS (comps',conns') inputSource
                    let nodeLst = createNodetoCompsList (comps'',conns')

                    let vs = 
                        comps''
                        |> List.filter (fun c-> match c.Type with |VoltageSource _ -> true |_ -> false)
                        |> List.length

                    let opamps = comps'' |> List.filter (fun c-> c.Type=Opamp) |> List.length

                    // setup MNA matrix and VecB
                    let n = List.length nodeLst - 1 + vs + opamps
                    let arr = Array.create n 0.0
                    let matrix = Array.create n arr
                    let vecB =
                        Array.create n {Re=0.0;Im=0.0}
                        |> Array.mapi (fun i v -> (calcVectorBElement (i+1) comps'' nodeLst [||]))

                    // find magnitude and phase using the source frequency 
                    let res = acAnalysis matrix nodeLst comps'' vecB (2.*System.Math.PI*f) outputNode |> complexCToP
                    {comp with Type = VoltageSource (Sine (a*res.Mag,o*dcGain,f,p+res.Phase))}


                |_ -> failwithf "Impossible"
            |None -> failwithf "No Voltage Source present in the circuit"


        let HFGain = findHFGain inputNode outputNode

        let tau = findTau ()
        let f = match yssAsVS.Type with |VoltageSource (Sine (_,_,f,_)) -> f |_ -> 0.

        let alpha = HFGain * findInputAtTime vs 0. - findInputAtTime (Some yssAsVS) 0.
    
        let dts = if f=0. then [0.0..(tau/20.)..tau*10.] else [0.0..(1./(40.*f))..5./f]
        let ytr,yss =
            dts
            |> List.map (fun t->
                let y_tr = alpha*exp(-t/tau)
                let y_ss = findInputAtTime (Some yssAsVS) t
                (y_tr,y_ss)
            )
            |> List.unzip
        {TimeSteps=dts;Transient=ytr;SteadyState=yss;Tau=tau;Alpha=alpha;HFGain=HFGain;DCGain=dcGain}


    |_ -> DCTimeAnalysis (comps',conns') inputNode outputNode
     