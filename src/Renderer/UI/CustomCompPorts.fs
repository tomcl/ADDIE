module CustomCompPorts

(*
This module provides some functions that ensure consistency of instantiated custom components when changes are
made to ports in the underlying sheet. A dialog is presented which allows instantiated components ports to
be updated correctly, with best efforts attempt to keep existing connections to each instance where ports remain
the same or where it can safely be deduced how ports have been renamed.

The code potentially makes changes to every sheet in the project in the model, and writes out these changes to disk.
*)

open Fulma
open Fable.React
open Fable.React.Props

open Helpers
open JSHelpers
open ModelType
open ModelHelpers
open CommonTypes
open FilesIO
open Extractor
open PopupView
open FileMenuView


let printSheetNames (model:Model) =
    model.CurrentProj
    |> Option.map (fun p -> 
        printf $"SHEETS:{p.LoadedComponents |> List.map (fun ldc -> ldc.Name)}--->{p.OpenFileName}")
    |> ignore


let getCorrectFileName (project:Project) = 
    match project.WorkingFileName with
    |Some name -> name
    |None -> project.OpenFileName


//--------------------------------------------------------------------------------------------//
//--------------------------------------------------------------------------------------------//
//-------------------------------New-style project update and saving--------------------------//
//--------------------------------------------------------------------------------------------//



/// Save any changed sheets to disk in the project directory
let syncLoadedComponentsToDisk newProj oldProj =
    let needToSave ldc' ldc =
       (not <| compareCanvas 10. ldc'.CanvasState ldc.CanvasState)
    let saveToDisk ldc =
        let state = ldc.CanvasState
        let sheetInfo = {Form=ldc.Form;Description=ldc.Description}
        saveStateToFile newProj.ProjectPath ldc.Name (state,sheetInfo)
        |> ignore
        removeFileWithExtn ".dgmauto" oldProj.ProjectPath ldc.Name

    let nameOf sheet (ldc:LoadedComponent) = ldc.Name = sheet
    let oldLDCs = oldProj.LoadedComponents
    let newLDCs = newProj.LoadedComponents
    let sheets = List.distinct (List.map (fun ldc -> ldc.Name) (oldLDCs @ newLDCs))
    let sheetMap = 
        sheets
        |> List.map (fun sheet -> sheet, (List.tryFind (nameOf sheet) newLDCs, List.tryFind (nameOf sheet) oldLDCs))
        |> Map.ofList
    sheetMap
    |> Map.iter (fun name (optLdcNew, optLdcOld) ->
        match optLdcNew,optLdcOld with
        | Some ldcNew, Some ldcOld when needToSave ldcNew ldcOld ->
            saveToDisk ldcOld
        | Some _, Some _ -> ()
        | None, Some ldcOld -> 
            removeFileWithExtn ".dgm" oldProj.ProjectPath ldcOld.Name
        | Some ldcNew, None -> 
            saveToDisk ldcNew
        | None, None -> failwithf "What? Can't happen")

/// Return new model with project updated as per update function.
/// If p.LoadedComponents data is changed, for each sheet that is different
/// the sheet will be saved to disk.
/// This function should be used consistently to keep disk and project data
/// correct.
let updateProjectFiles (saveToDisk:bool) (update: Project -> Project) (model: Model) =
    match model.CurrentProj with
    | None -> model // do nothing in this case
    | Some p ->
        let p' = update p
        if saveToDisk then 
            syncLoadedComponentsToDisk p' p // write out to disk as needed
        {model with CurrentProj = Some p'}

