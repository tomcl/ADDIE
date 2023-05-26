module CanvasStateAnalyser

open CommonTypes


//////////  CANVAS STATE ANALYSER    ////////


/// Helper function to find the components between two nodes
/// using the NodeToCompsList
let findComponentsBetweenNodes (node1:int, node2:int) (nodeToCompsList:(Component*int option) list list) =
    let comps1 = nodeToCompsList[node1] 
    let comps2 = nodeToCompsList[node2]
    comps1
    |> List.filter (fun (c1,no) -> (List.exists (fun (c2:Component,no) -> c1.Id = c2.Id) comps2))

let findNodesOfComp (nodeToCompsList:(Component*int option) list list) compId =
    let pair =
        nodeToCompsList
        |> List.mapi (fun i localNode -> 
            localNode |> List.collect (fun (c,_)->
                match c.Id=compId with
                |true -> [i]
                |false -> []
            )
        )
        |> List.collect id

    match List.length pair with
    |2 when pair[0] <> pair[1] -> (pair[0],pair[1])
    |_ -> failwithf "Wrong parsing of circuit, cannot identify two nodes between a component"

let findConnectionsOnNode (nodeToCompsList:(Component*int option) list list) (index:int) (conns:Connection list) =
    
    let nodeComps = nodeToCompsList[index]

    conns
    |> List.filter (fun conn ->
        List.exists (fun (c:Component,pNo) -> 
            match pNo with
            |Some portNumber -> c.Id=conn.Source.HostId && c.IOPorts[portNumber].Id=conn.Source.Id
            |_ -> c.Id=conn.Source.HostId) nodeComps 
        && List.exists (fun (c:Component,pNo) -> 
            match pNo with
            |Some portNumber -> c.Id=conn.Target.HostId && c.IOPorts[portNumber].Id=conn.Target.Id
            |_ -> c.Id=conn.Target.HostId) nodeComps 
    )


let findNodeLocation (connsOnNode: Connection list) =
    let extractX (vertix: (float*float*bool)) =
        vertix |> (fun (x,_,_) -> x)
    
    let extractY (vertix: (float*float*bool)) =
        vertix |> (fun (_,y,_) -> y)

    
    let removeDuplicates (vertices: (float*float*bool) list) =
        vertices
        |> List.indexed
        |> List.collect (fun (i,(x,y,z)) -> 
            match i with
            |0 -> [(x,y,z)]
            |_ ->
                if (extractX vertices[i-1] = x && extractY vertices[i-1] = y) then
                    []
                else [(x,y,z)]
        )
        
    let removeSticks (vertices: (float*float*bool) list) =
        vertices
        |> List.removeAt 0
        |> List.removeAt (List.length vertices-2)

    let findMid (vertices: (float*float*bool) list) =
        let mid = List.length vertices / 2
        {X= extractX vertices[mid]; Y=extractY vertices[mid]}

    match List.length connsOnNode with
    |0 -> failwithf "Impossible"
    |1 -> 
        connsOnNode[0].Vertices |> removeDuplicates |> removeSticks |> findMid
    |_ ->
        connsOnNode
        |> List.map (fun x -> x.Vertices |> removeDuplicates |> removeSticks)
        |> List.map ( 
            List.map (fun v -> {X=extractX v; Y= extractY v}))
        |> List.collect id
        |> List.countBy id
        |> List.sortBy (fun (pos,count) -> count)
        |> List.head
        |> (fun (pos,count) -> 
            if count <> 1 then
                pos
            else
                connsOnNode[0].Vertices |> removeDuplicates |> removeSticks |> findMid
        )





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
let createNodetoCompsList(comps:Component list,conns: Connection list) =
    
    let ground = (List.tryFind (fun (c:Component) -> c.Type = Ground) (comps:Component list))
        
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


    match ground with
    |Some g -> searchCurrentCanvasState [] [g.IOPorts[0]] [[]]
    |None -> []

//////////////////////////////////////////////////////////


///////////// CANVAS STATE ANALYSER - ERROR CHECKER ////////////////////////
let checkCanvasStateForErrors (comps,conns) =
    let nodeLst = createNodetoCompsList (comps,conns)
    let allCompsOfNodeLst = nodeLst |> List.collect (id) |> List.distinctBy (fun (c,pn) -> c.Id)

    let findLabelFromId id =
        let c = comps |> List.find (fun c->c.Id=id)
        c.Label

    let extractVS comps =
        comps 
        |> List.filter (fun c -> 
            match c.Type with 
            |VoltageSource _ -> true 
            |_ -> false)

    let extractCS comps =
        comps 
        |> List.filter (fun c -> 
            match c.Type with 
            |CurrentSource _ -> true 
            |_ -> false)

    let extractPortIds =
        comps
        |> List.collect (fun c->
            c.IOPorts |> List.collect (fun p -> [p.Id,c])
        )


    let checkGroundExistance =
        match List.exists (fun c->c.Type = Ground) comps with
        |true -> []
        |false ->
            [{
                Msg = sprintf "There is no Ground present in the circuit"
                ComponentsAffected = comps |> List.map (fun c->ComponentId c.Id)
                ConnectionsAffected = []
            }]

    let checkAllCompsConnected =
        comps
        |> List.collect (fun c->
            match List.exists (fun (comp:Component,pn) -> comp.Id = c.Id) allCompsOfNodeLst with
            |true -> []
            |false ->
                [{
                    Msg = sprintf "Component %s is not connected with the main graph" c.Label
                    ComponentsAffected = [ComponentId c.Id]
                    ConnectionsAffected = []
                }]        
        )

    let checkAllPortsConnected =
        extractPortIds
        |> List.collect (fun (pId,comp)->
            let asTarget = List.exists (fun conn -> conn.Target.Id = pId) conns
            let asSource = List.exists (fun conn -> conn.Source.Id = pId) conns
            match asTarget || asSource with
            |true -> []
            |false -> 
                [{
                    Msg = sprintf "Every component port must be connected, but a port of %s is not connected" comp.Label
                    ComponentsAffected = [ComponentId comp.Id]
                    ConnectionsAffected = []
                }] 
       )
 
    let vs = comps |> extractVS
    
    
    let checkNoParallelVS = 
        vs
        |> List.collect (fun comp ->
            let pair = findNodesOfComp nodeLst comp.Id
            let vsOfPair =
                findComponentsBetweenNodes pair nodeLst        
                |> List.map (fst)
                |> extractVS
    
            match List.length vsOfPair with 
            |0 |1 -> []
            |_ ->
                [{
                Msg = sprintf "Voltage sources are connected in parallel between nodes %i and %i" (fst pair) (snd pair)
                ComponentsAffected = vsOfPair |> List.map (fun c->ComponentId c.Id)
                ConnectionsAffected = []
                }]  
            )
    
    let checkNoSeriesCS = 
        nodeLst
        |> List.indexed
        |> List.collect (fun (i, node) ->
            let csOfNode =
                node
                |> List.map fst
                |> extractCS
            
            match (List.length csOfNode) = (List.length node) with 
            |false -> []
            |true ->
                [{
                Msg = sprintf "Current sources are connected in series on node %i" i
                ComponentsAffected = csOfNode |> List.map (fun c->ComponentId c.Id)
                ConnectionsAffected = []
                }]  
            )
    
    let checkNoDoubleConnections =
        let conns'=conns|> List.map (fun c -> {c with Vertices = []})
        List.allPairs conns' conns'
        |> List.collect (fun (c1,c2) ->
            if ((c1.Source.Id = c2.Source.Id && c1.Target.Id = c2.Target.Id)
                || (c1.Source.Id = c2.Target.Id && c1.Target.Id = c2.Source.Id))
                && c1.Id <> c2.Id then
                    [{
                    Msg = sprintf "Duplicate connection between %s and %s" (findLabelFromId c1.Source.HostId) (findLabelFromId c1.Target.HostId) 
                    ComponentsAffected = [] 
                    ConnectionsAffected = [ConnectionId c1.Id; ConnectionId c2.Id]
                    }]  
            else 
                []
        )

    let checkNoLoopConns =
        conns
        |> List.collect (fun conn ->
            match conn.Source.HostId = conn.Target.HostId with
            |false -> []
            |true ->
                let comp = List.find (fun (c:Component) ->c.Id = conn.Source.HostId) comps
                match comp.Type with
                |Opamp -> []
                |_ ->
                    [{
                    Msg = sprintf "Loop connection on %s" (findLabelFromId conn.Source.HostId) 
                    ComponentsAffected = [] 
                    ConnectionsAffected = [ConnectionId conn.Id]
                    }]    
        )
 
    checkGroundExistance
    |> List.append checkAllCompsConnected
    |> List.append checkAllPortsConnected
    |> List.append checkNoParallelVS
    |> List.append checkNoSeriesCS
    |> List.append checkNoDoubleConnections
    |> List.append checkNoLoopConns
    

/////////////////////////////////////////////////////////////