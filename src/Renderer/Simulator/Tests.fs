module Test
open CommonTypes

type TestCase = {
    CanvasState: CanvasState
    TestedSim: SimSubTab
    DCRes: float array
    ACTimeInput: string
    ACTimeOutput: string
    ACTimeRes: Map<int,float>
}


/// Simple Potential Divider
let state1: CanvasState = 
        [
            {Id = "R4k";Type = Resistor (4000,"");Label = "R4k";IOPorts = [{Id = "R4kPort0";PortNumber = Some 0;HostId = "R4k" };{Id = "R4kPort1";PortNumber = Some 1;HostId = "R4k" }];X = 0; Y = 0; H = 0;W = 0; SymbolInfo = None }
            {Id = "R1k";Type = Resistor (1000,"");Label = "R1k";IOPorts = [{Id = "R1kPort0";PortNumber = Some 0;HostId = "R1k" };{Id = "R1kPort1";PortNumber = Some 1;HostId = "R1k" }];X = 0; Y = 0; H = 0;W = 0; SymbolInfo = None }
            {Id = "VS5DC";Type = VoltageSource (DC 5) ;Label = "VS5DC";IOPorts = [{Id = "VS5DCPort0";PortNumber = Some 0;HostId = "VS5DC" };{Id = "VS5DCPort1";PortNumber = Some 1;HostId = "VS5DC" }];X = 0; Y = 0; H = 0;W = 0; SymbolInfo = None }
            {Id = "G1";Type = Ground ;Label = "G1";IOPorts = [{Id = "G1Port0";PortNumber = Some 0;HostId = "G1" }];X = 0; Y = 0; H = 0;W = 0; SymbolInfo = None }
        ],
        [
            {Id="conn0";Source={Id = "G1Port0";PortNumber = Some 0;HostId = "G1" };Target={Id = "VS5DCPort1";PortNumber = Some 1;HostId = "VS5DC" }; Vertices=[]}
            {Id="conn1";Source = {Id = "VS5DCPort0";PortNumber = Some 0;HostId = "VS5DC" } ; Target = {Id = "R1kPort0";PortNumber = Some 0;HostId = "R1k" } ; Vertices=[]}
            {Id="conn1";Source = {Id = "R1kPort1";PortNumber = Some 1;HostId = "R1k" } ; Target = {Id = "R4kPort0";PortNumber = Some 0;HostId = "R4k" } ; Vertices=[]}
            {Id="conn1";Source = {Id = "R4kPort1";PortNumber = Some 1;HostId = "R4k" } ; Target = {Id = "G1Port0";PortNumber = Some 0;HostId = "G1" } ; Vertices=[]}
        ]


    //{Id="conn1";Source =  ; Target =  ; Vertices=[]}

let testCase1 = {CanvasState=state1; TestedSim = DCsim; DCRes = [|5.;4.;-0.001|]; ACTimeInput="";ACTimeOutput=""; ACTimeRes= Map.empty<int,float>}
    

(*
let runTestCases () =
    
    let allTestCases = [testCase1]
    allTestCases
    |> List.map (fun tc ->
        match tc.TestedSim with
        |DCsim ->
            let mna,_,_,_ = Simulation.modifiedNodalAnalysisDC tc.CanvasState []
            mna = tc.DCRes
        |ACsim ->
            false //PENDING
        |TimeSim ->
            false //PENDING
        )

*)
let runTestCases () =
    
    let allTestCases = [testCase1]
    allTestCases
    |> List.map (fun tc ->
        match tc.TestedSim with
        |DCsim ->
            match (Simulation.modifiedNodalAnalysisDC tc.CanvasState []) with
             | Error m -> false
             | Ok (mna,_,_,_) ->  mna = tc.DCRes
        |ACsim ->
            false //PENDING
        |TimeSim ->
            false //PENDING
        )
