(*
    FilesIO.fs

    Utility functions to interact with files.
*)

module FilesIO
open Fulma
open Fable.React.Props
open Helpers
open CommonTypes
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open ElectronAPI

open Node
open EEExtensions
open Fable.SimpleJson
open JSHelpers

[<Emit("process.cwd()")>]
let getCWD (u:unit): string = jsNative

// [<Emit("__static")>]
// let staticDir() :string = jsNative

/// absolute path to repo directory ./static
/// NB this path is not fixed (even as relative path) between
/// production and dev builds, so this must be used to access static
/// assets.
// let staticFileDirectory = staticDir()


let pathJoin args = path.join args
let baseName filePath = path.basename filePath


let dirName filePath = path.dirname filePath
let ensureDirectory dPath =
    if (not <| fs.existsSync (U2.Case1 dPath)) then 
        fs.mkdirSync(dPath);

let pathWithoutExtension filePath =
    let ext = path.extname filePath
    filePath 
    |> Seq.rev
    |> Seq.skip ext.Length
    |> Seq.rev
    |> String.ofSeq

let baseNameWithoutExtension =
    pathWithoutExtension >> baseName

let fileNameIsBad name = 
    name 
    |> Seq.filter (fun ch -> not (ch = ' ' || System.Char.IsLetter ch || System.Char.IsDigit ch))
    |> Seq.isEmpty
    |> not

let filePathIsBad = 
    baseNameWithoutExtension >> fileNameIsBad

let fileExistsWithExtn extn folderPath baseName =
    let path = path.join [| folderPath; baseName + extn |]
    fs.existsSync (U2.Case1 path)

let tryReadFileSync fPath =
    if not <| fs.existsSync (U2.Case1 fPath) then
        Error $"Error: file {fPath} does not exist"
    else    
    fs.readFileSync(fPath, "utf8")
    |> Ok


/// Write base64 encoded data to file.
/// Create file if it does not exist.
let writeFileBase64 path data =
    let options = createObj ["encoding" ==> "base64"] |> Some
    try
        fs.writeFileSync(path, data, options)
        Ok ()
    with
        | e -> Result.Error $"Error '{e.Message}' writing file '{path}'"   

/// Write utf8 encoded data to file.
/// Create file if it does not exist.
let writeFile path data =
    try
        let options = createObj ["encoding" ==> "utf8"] |> Some
        fs.writeFileSync(path, data, options)
        Ok ()
    with
        | e -> Result.Error $"Error '{e.Message}' writing file '{path}'"

/// read file names from directory: returning [] on any error.
let readFilesFromDirectory (path:string) : string list =
    if fs.existsSync (U2.Case1 path) then
        try 
            fs.readdirSync(U2.Case1 path)
            |> Seq.toList
        with 
            | _ -> []
    else
        []

let hasExtn extn fName =
    (String.toLower fName).EndsWith (String.toLower extn)


let readFilesFromDirectoryWithExtn (path:string) (extn:string) : string list =
    readFilesFromDirectory path
    |> List.filter (fun name -> hasExtn extn name)

let removeExtn extn fName = 
    if hasExtn extn fName
    then Some fName[0..(fName.Length - extn.Length - 1)]
    else None

/// returns the list of backup files in descending chronological order.
let backupFileData (path:string) (baseName: string) =
    readFilesFromDirectory path
    |> List.filter (fun fn -> String.startsWith (baseName + "-") fn)
    |> List.map (fun fn -> 
            String.splitString [|"-"|] fn 
            |> Array.tryItem 1
            |> Option.bind (String.tryParseWith System.Int32.TryParse)
            |> fun n -> n,fn)
    |> List.sortDescending



/// returns the sequence number and name of the most recent (highest sequence number) backup file
let latestBackupFileData (path:string) (baseName: string) =
    backupFileData path baseName
    |> List.tryHead
    |> Option.bind (function 
        | None,_ -> None 
        | Some n, fn -> Some(n, fn))



/// read canvas state from file found on filePath (which includes .dgm suffix etc).
/// return Error if file does not exist or cannot be parsed.
let private tryLoadStateFromPath (filePath: string) =
    if not (fs.existsSync (U2.Case1 filePath)) then
        Result.Error <| sprintf "Can't read file from %s because it does not seem to exist!" filePath      
    else
        try
            Ok (fs.readFileSync(filePath, "utf8"))
        with
            | e -> Result.Error $"Error {e.Message} reading file '{filePath}'"

        |> Result.map jsonStringToState
        |> ( function
            | Error msg  -> Result.Error <| sprintf "could not convert file '%s' to a valid addie design sheet. Details: %s" filePath msg
            | Ok res -> Ok res)

let makeData aWidth dWidth makeFun =
    let truncate n =
        match dWidth with
        | 64 -> n
        | w -> ((1UL <<< w) - 1UL) &&& n
        |> int64
    let a = aWidth / 2
    let inp = [|0..(1 <<< a) - 1|]
    Array.allPairs inp inp
    |> Array.map (fun (x,y) -> int64 ((int64 x <<< a) + int64 y), truncate (uint64 (makeFun x y)))
    |> Map.ofArray



let jsonStringToMem (jsonString : string) =
     Json.tryParseAs<Map<int64,int64>> jsonString


let getBaseNameNoExtension filePath =
    let name = baseName filePath
    match name.Split '.' |> Seq.toList with
    | [] -> failwithf "what? split at . in a filename should never return empty list"
    | [name] -> name // No dots found.
    | firstSplit :: splits ->
        // Quite ugly but works.
        let rest =
            ("", [0..splits.Length - 2]) ||> List.fold (fun baseName i ->
                name + "." + splits[i]
            )
        firstSplit + rest

let private projectFileFilters =
    createObj !![
        "name" ==> "Addie project file"
        "extensions" ==> ResizeArray [ "dprj" ]
    ] 
    |> unbox<FileFilter> 
    |> Array.singleton

let private ramFileFilters =
    createObj !![
        "name" ==> "Memory contents File"
        "extensions" ==> ResizeArray [ "ram" ]
    ] 
    |> unbox<FileFilter> 
    |> Array.singleton

let private projectFilters =
    createObj !![ 
        "name" ==> "Addie project"   
        "extensions" ==> ResizeArray [ "" ]
    ]
    |> unbox<FileFilter>
    |> Array.singleton


/// Ask the user to choose a project file, with a dialog window.
/// Return the folder containing the chosen project file.
/// Return None if the user exits withouth selecting a path.
let askForExistingProjectPath (defaultPath: string option) : string option =
    let options = createEmpty<OpenDialogSyncOptions>
    options.filters <- Some (projectFileFilters |> ResizeArray)
    options.defaultPath <-
        defaultPath
        |> Option.defaultValue (electronRemote.app.getPath ElectronAPI.Electron.AppGetPath.Documents)
        |> Some
    let w = electronRemote.getCurrentWindow()
    electronRemote.dialog.showOpenDialogSync(w,options)
    |> Option.bind (
        Seq.toList
        >> function
        | [] -> Option.None
        | p :: _ -> Some <| path.dirname p
    )



/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let rec askForNewProjectPath (defaultPath:string option) : string option =
    let options = createEmpty<SaveDialogSyncOptions>
    options.filters <- Some (projectFilters |> ResizeArray)
    options.title <- Some "Enter new Addie project directory and name"
    options.nameFieldLabel <- Some "New project name"
    options.defaultPath <- defaultPath
    options.buttonLabel <- Some "Create Project"
    options.properties <- Some [|
        SaveDialogOptionsPropertiesArray.CreateDirectory
        SaveDialogOptionsPropertiesArray.ShowOverwriteConfirmation
        |]
    match electronRemote.getCurrentWindow() with
    | w ->
        electronRemote.dialog.showSaveDialogSync(options)
        |> Option.bind (fun dPath ->
            let dir = dirName dPath
            let files = fs.readdirSync <| U2.Case1 dir
            if Seq.exists (fun (fn:string) -> fn.EndsWith ".dprj") files
            then
                electronRemote.dialog.showErrorBox(
                    "Invalid project directory",
                    "You are trying to create a new Addie project inside an existing project directory. \
                     This is not allowed, please choose a different directory")
                askForNewProjectPath defaultPath
            
            else
                Some dPath)
    
    


    
let tryCreateFolder (path : string) =
    if Seq.exists (fun (ch:char) -> (not (System.Char.IsLetterOrDigit ch || ch = '_'))) (baseName path) then 
        Result.Error <| "File or project names must contain only letters, digits, or underscores"
    else
        try
            Result.Ok <| fs.mkdirSync path
        with
            | ex -> Result.Error <| $"Can't create folder '{path}': {ex.Message}"


/// Asyncronously remove file.
/// ignore if file does not exist
let removeFileWithExtn extn folderPath baseName  =
    let path = path.join [| folderPath; baseName + extn |]
    if fs.existsSync (U2.Case1 path) then
        try 
            fs.unlink (U2.Case1 path, ignore) // Asynchronous.
        with
            | _ -> ()
    else
        ()

let renameFile extn folderPath baseName newBaseName =
    let oldPath = path.join [| folderPath; baseName + extn |]
    let newPath = path.join [| folderPath; newBaseName + extn |]
    if fs.existsSync <| U2.Case1 oldPath then
        try
            Ok <| fs.renameSync (oldPath, newPath) // synchronous.
        with
            | e -> Error  $"Rename of '{baseName}' in '{folderPath}' failed"
    elif extn = ".dgm" then
        Error $"Error: The file '{baseName}{extn} appears to have been removed"
    else
        Ok ()

let removeFile (folderPath:string) (baseName:string) = removeFileWithExtn ".dgm" folderPath baseName

let removeAutoFile folderPath baseName =
    let path = path.join [| folderPath; baseName + ".dgmauto" |]
    fs.unlink (U2.Case1 path, ignore) // Asynchronous.


/// Save a PNG file (encoded base64, as from draw2d)
/// Overwrite existing file if needed
let savePngFile folderPath baseName png = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".png" |]
    writeFileBase64 path png



/// Save state to normal file. Automatically add the .dgm suffix.
let saveStateToFile folderPath baseName state = // TODO: catch error?
    let path = pathJoin [| folderPath; baseName + ".dgm" |]
    let data = stateToJsonString state
    writeFile path data

/// Create new empty diagram file. Automatically add the .dgm suffix.
let createEmptyDgmFile folderPath baseName =
    saveStateToFile folderPath baseName (([],[]), {Form=Some User;Description=None})

let stripVertices (conn: LegacyCanvas.LegacyConnection) =
    {conn with Vertices = []}

let magnifySheet magnification (comp: LegacyCanvas.LegacyComponent) =
    {comp with 
        X = magnification * (comp.X + comp.W / 2. ); 
        Y = magnification * (comp.Y + comp.H/2.)
        H = -1 // overwritten correctly by Sheet based on componnet type
        W = -1 // as above
    }


/// Update from old component types to new
/// In addition do some sanity checks
/// The standard way to add functionality to an existing component is to create a new
/// component type, keeping the old type. Then on reading sheets from disk both new and old
/// will be correctly read. This function will be called on load and will convert from the old
/// type to the new one so that the rest of addie need only process new types, but compatibility
/// with saved old types remains.
let getLatestComp (comp: Component) =
    comp


/// Interface function that can read old-style circuits (without wire vertices)
/// as well as new circuits with vertices. Old circuits have an expansion parameter
/// since new symbols are larger (in units) than old ones.
let getLatestCanvas state =
    let oldCircuitMagnification = 1.25
    //let stripConns (canvas: LegacyCanvas.LegacyCanvasState) =
    //    let (comps,conns) = canvas
    //    let noVertexConns = List.map stripVertices conns
    //    let expandedComps = List.map (magnifySheet oldCircuitMagnification) comps
    //    (expandedComps, noVertexConns)
    //    |> legacyTypesConvert
    let comps,conns =
        match state  with
        | CanvasAndSheetInfo (canvas,_,_) -> canvas
    List.map getLatestComp comps, conns


/// load a component from its canvas and other elements
let makeLoadedComponentFromCanvasData (canvas: CanvasState) filePath timeStamp (sheetInfo:SheetInfo) =
    let projectPath = path.dirname filePath
    let io = Extractor.parseDiagramSignature canvas
    //printfn "parsed component"
    //printfn "ram changes processed"
    let form,description = sheetInfo.Form,sheetInfo.Description
    let ldc =
        {
            Name = getBaseNameNoExtension filePath
            TimeStamp = timeStamp
            FilePath = filePath
            CanvasState = canvas
            IOLabels = io
            Form = form
            Description = description
        }
    ldc


/// Make a loadedComponent from the file read from filePath.
/// Return the component, or an Error string.
let tryLoadComponentFromPath filePath : Result<LoadedComponent, string> =
    match tryLoadStateFromPath filePath with
    | Result.Error msg  
    | Ok (Result.Error msg) ->
        Error <| sprintf "Can't load component %s because of Error: %s" (getBaseNameNoExtension filePath)  msg
    | Ok (Ok state) ->
        let canvas = getLatestCanvas state
        makeLoadedComponentFromCanvasData 
            canvas
            filePath 
            state.getTimeStamp 
            state.getSheetInfo
        |> Result.Ok



type LoadStatus =
    | Resolve  of LoadedComponent * LoadedComponent
    | OkComp of LoadedComponent
    | OkAuto of LoadedComponent

    
/// load all files in folderpath. Return Ok list of LoadStatus or a single Error.
let loadAllComponentFiles (folderPath:string)  = 
    let x = 
        try
            Ok <| fs.readdirSync (U2.Case1 folderPath)
        with
        | e -> Error <| sprintf "Error reading Addie project directory at '%s: %A" folderPath e
    match x with
    | Error msg -> Error msg
    | Ok x ->
        x
        |> Seq.toList
        |> List.filter (path.extname >> ((=) ".dgm"))
        |> List.map (fun fileName ->
                if fileNameIsBad (pathWithoutExtension fileName)
                then
                    Error <| sprintf @"Can't load file name '%s' from project '%s' because it contains incorrect characters.\n \
                    File names used as sheets must contain only alphanumeric and space characters before the '.dgm' extension" fileName folderPath
                else 
                    let filePath = path.join [| folderPath; fileName |]
                    printfn $"loading {fileName}"
                    let ldComp =  filePath |> tryLoadComponentFromPath
                    let autoComp = filePath + "auto" |> tryLoadComponentFromPath
                    printfn $"{fileName} Loaded"
                    match (ldComp, autoComp) with
                    | Ok ldComp, Ok autoComp when ldComp.TimeStamp < autoComp.TimeStamp ->
                        Resolve(ldComp,autoComp) |> Ok
                    | Ok ldComp, _ -> 
                        OkComp ldComp |> Ok
                    | Error _, Ok autoComp ->
                        OkAuto autoComp |> Ok
                    | Error msg, _ -> Error msg
            )
        |> tryFindError

/// Ask the user a new project path, with a dialog window.
/// Return None if the user exits withouth selecting a path.
let rec askForNewFile (projectPath: string) : string option =
    let options = createEmpty<SaveDialogSyncOptions>
    options.filters <- Some (ramFileFilters |> ResizeArray)
    options.defaultPath <- Some projectPath
    options.title <- Some "Enter new file name"
    options.nameFieldLabel <- Some "New file name"
    options.buttonLabel <- Some "Save memory content to file"
    options.properties <- Some [|
        SaveDialogOptionsPropertiesArray.ShowOverwriteConfirmation
        |] 
    match electronRemote.getCurrentWindow() with
    | w ->
        electronRemote.dialog.showSaveDialogSync(options)
        
let saveAllProjectFilesFromLoadedComponentsToDisk (proj: Project) =
    proj.LoadedComponents
    |> List.iter (fun ldc ->
        let name = ldc.Name
        let state = ldc.CanvasState
        let sheetInfo = {Form=ldc.Form;Description=ldc.Description}
        saveStateToFile proj.ProjectPath name (state,sheetInfo) |> ignore
        removeFileWithExtn ".dgmauto" proj.ProjectPath name)

let openWriteDialogAndWriteMemory mem path =
    match askForNewFile path with
    | None -> 
        None
    | Some fpath ->
        Some fpath
    




