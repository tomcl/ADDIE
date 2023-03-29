module DCAnalysis

open CommonTypes

/// Helper function to find the components between two nodes
/// using the NodeToCompsList
let findComponentsBetweenNodes (node1:int) (node2:int) (nodeToCompsList:(Component*int option) list list) =
    let comps1 = nodeToCompsList[node1] 
    let comps2 = nodeToCompsList[node2]

    comps1
    |> List.filter (fun (c1,no) -> (List.exists (fun (c2:Component,no) -> c1.Id = c2.Id) comps2))



/// Given a port on a component, return the other port on this component 
/// (If it exists: Option, otherwise None)
let findOtherEndPort (port:Port) (comps: Component List) : Port option =
    let compOfPort:Component option = List.tryFind (fun c -> c.Id = port.HostId ) comps
    match compOfPort with
    |None -> None
    |Some comp ->
        List.tryFind (fun p -> p.Id <> port.Id) comp.IOPorts


/// Creates a (Component*int option) list list where each element of the top list
/// represents one node and the inner list contains the components
/// that "touch" that node along with the portNumber that touches it (for VS/CS direction). 
/// PENDING: 1 COMMON GROUND
let createNodetoCompsList(comps:Component list,conns: Connection list) =
    let ground = 
        match (List.tryFind (fun (c:Component) -> c.Type = Ground) (comps:Component list)) with
        |Some c -> c
        |None -> failwithf "No ground present in the circuit"
   

    /// Function to find the PortNumber of a port which comes from Source/Target of a connection
    let findPortNo (port:Port) (comps:Component list) =
        let comp = comps |> List.find(fun c-> c.Id = port.HostId)
        comp.IOPorts
        |> List.find (fun p -> p.Id = port.Id)
        |> (fun p -> p.PortNumber)
        

    let rec searchCurrentNode (visited:Port list) (toVisitLocal:Port list) (nodeToCompLocalList: (Component*int option) list)=
        match List.isEmpty toVisitLocal with
        |true -> (nodeToCompLocalList,visited)
        |false ->
            let connectedToCurrentPort = 
                List.filter (fun conn -> (conn.Source.Id = toVisitLocal[0].Id || conn.Target.Id = toVisitLocal[0].Id)) conns
                |> List.map(fun c ->
                    match c.Target.Id with
                    |i when i=toVisitLocal[0].Id -> c.Source
                    |_ -> c.Target          
                )
                |> List.collect (fun port -> if (List.exists (fun (x:Port) -> x.Id = port.Id) visited) then [] else [port])

                
            let visited' = List.append visited [toVisitLocal[0]]
            let toVisit' = List.append (List.removeAt 0 toVisitLocal) (connectedToCurrentPort)
            
            let localList' = 
                List.append 
                    nodeToCompLocalList 
                    (List.map (fun p-> 
                        (List.find (fun c->c.Id = p.HostId) comps), (findPortNo p comps) )
                        connectedToCurrentPort
                    )

            searchCurrentNode visited' toVisit' localList'
    
            
    and searchCurrentCanvasState visited (toVisit:Port list) (nodeToCompList:(Component*int option) list list) =

        match List.isEmpty toVisit with
        |true -> List.removeAt 0 nodeToCompList //first element is empty
        |false -> 
            let toVisitFirstComp = List.find (fun (c:Component) -> c.Id = toVisit[0].HostId) comps
            let (newNodeList,visited') = searchCurrentNode visited [toVisit[0]] []
            
            
            let visitedSeq = List.toSeq visited
            let diff = visited' |> List.toSeq |> Seq.except visitedSeq |> Seq.toList
            
            let oppositePorts = 
                List.map (fun port -> findOtherEndPort port comps) diff
                |> List.collect (fun p -> if p = None then [] else [(Option.get p)])
            
            let toVisit' = List.append (List.removeAt 0 toVisit) oppositePorts

            let nodeToCompList' = 
                match List.isEmpty newNodeList with
                |true -> nodeToCompList
                |false -> List.append nodeToCompList [(List.append newNodeList [(toVisitFirstComp, (findPortNo toVisit[0] comps) )])]

            searchCurrentCanvasState visited' toVisit' nodeToCompList'


    searchCurrentCanvasState [] [ground.IOPorts[0]] [[]]


let calcVectorBElement row (nodeToCompsList:(Component*int option) list list) =
    let nodesNo = List.length nodeToCompsList
    
    let findNodeTotalCurrent currentNode = 
        (0.0, currentNode) ||> List.fold (fun s (comp,no) ->
            printfn "ROW= %i, comp= %A, no= %A" row comp no
            match (comp.Type,no) with
            |CurrentSource (v,_),Some 0 -> s+v
            |CurrentSource (v,_), Some 1 -> s-v
            |_ -> s
        )

    let allDCVoltageSources = 
        nodeToCompsList
        |> List.removeAt 0
        |> List.collect (fun nodeComps ->
            nodeComps
            |> List.collect (fun (comp,_) ->
                match comp.Type with
                |VoltageSource (DC (_)) -> [comp]
                |_ -> []
            )
        )
        |> List.distinctBy (fun c->c.Id)
                
    if row < nodesNo then
        let currentNode = nodeToCompsList[row]
        findNodeTotalCurrent currentNode
    else
        printfn "VoltageSources %A" allDCVoltageSources
        allDCVoltageSources[row-nodesNo]
        |> (fun c -> 
            match c.Type with
            |VoltageSource (DC v) -> v
            |_ -> failwithf "Impossible"
        )
    

    
let calcMatrixElementValue row col (nodeToCompsList:(Component*int option) list list) = 
    let findMatrixCompValue (comp,no) =
        match comp.Type with
        |Resistor (v,_) -> (1./v)
        |CurrentSource _ |VoltageSource _ -> 0.0
        |_ -> failwithf "Not Implemented yet"
    
    let nodesNo = List.length nodeToCompsList
    
    if (row < nodesNo) then
        if row=col then
            (0.0, nodeToCompsList[row]) 
            ||> List.fold(fun s c ->
                s + (findMatrixCompValue c)
            )

        else
            (0.0, findComponentsBetweenNodes row col nodeToCompsList)
            ||> List.fold(fun s c ->
                s - (findMatrixCompValue c)
            )
    else 
        if row=col then
            0.0
        else
            0.0 //CONTINUE


let modifiedNodalAnalysis (comps,conns) =
    let nodeLst = createNodetoCompsList (comps,conns)
    
    let vs = 
        comps
        |> List.filter (fun c-> match c.Type with |VoltageSource _ -> true |_ -> false)
        |> List.length

    let n = List.length nodeLst - 1 + vs

    //let matrix = 
    //    Array2D.create n n 0.0
    //    |> Array2D.mapi (fun i j v -> (calcMatrixElementValue (i+1) (j+1) nodeLst))
    printfn "n= %i" n

    let vecB =
        Array.create n 0.0
        |> Array.mapi (fun i v -> (calcVectorBElement (i+1) nodeLst))
    
    vecB



