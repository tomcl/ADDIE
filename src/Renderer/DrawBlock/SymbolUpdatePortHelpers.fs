module SymbolUpdatePortHelpers


open DrawModelType.SymbolT
open Optics
open Optic
open Operators


//////////////  Show Ports Helpers  /////////////////////


let showAllSymbolPorts _ sym = 
     set (appearance_ >-> showPorts_) ShowBoth sym 

let hideSymbolPorts _ sym = 
    set (appearance_ >-> showPorts_) ShowNone sym 

let showSymbolPorts sym =
    set (appearance_ >-> showPorts_) ShowBoth sym 

/////////////////////////////////////////////////////////

/// Given a model it shows all input ports and hides all output ports, then returns the updated model
let inline showAllPorts (model: Model) =
    let newSymbols = 
        model.Symbols
        |> Map.map showAllSymbolPorts

    { model with Symbols = newSymbols }



/// Given a model it hides all ports and returns the updated model
let inline deleteAllPorts (model: Model) =
    let updatedSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    { model with Symbols = updatedSymbols}

/// Given a model it shows all the specified components' ports and hides all the other ones
let inline showPorts (model: Model) compList =
    let resetSymbols = 
        model.Symbols
        |> Map.map hideSymbolPorts

    let addUpdatedSymbol prevSymbols sId =
        match Map.containsKey sId resetSymbols with
        | false -> prevSymbols
        | true ->
            prevSymbols |>
            Map.add sId (showSymbolPorts resetSymbols[sId])

    let newSymbols =
        (resetSymbols, compList)
        ||> List.fold addUpdatedSymbol

    { model with Symbols = newSymbols }

