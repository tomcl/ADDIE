module MathJsHelpers

open Fable.Core
open System
open CommonTypes

type MathsJS =
    abstract complex : float * float -> obj
    abstract reshape : ResizeArray<float> * ResizeArray<int> -> obj
    abstract reshape : obj * ResizeArray<int> -> ResizeArray<obj>
    abstract inv : obj -> obj 
    //abstract multiply : obj*obj -> ResizeArray<float>
    abstract multiply : obj*obj -> ResizeArray<obj>
    abstract divide : obj*obj -> obj
    abstract det : obj -> obj
    abstract isZero : obj -> bool
    abstract re: obj -> float
    abstract im: obj -> float
    abstract rationalize: string -> string
    abstract numeric : obj -> string
    //abstract atan: float -> float

[<ImportAll("mathjs")>]
let Maths: MathsJS = jsNative



/// ComplexC -> obj (complex in mathJS environment)
let toComplexJS (a:ComplexC) =
    (a.Re,a.Im) |> Maths.complex

/// obj (complex in mathJS environment) -> ComplexC
let toComplexF (a:obj) =
    {Re = (Maths.re a);Im = (Maths.im a)}

let tupleToComplex (a,b) = {Re=a;Im=b}

let complexCToP (a:ComplexC) = 
    let mag = sqrt (a.Re*a.Re + a.Im *a.Im)
    let phase = atan (a.Im / a.Re)
    let phase' =
        match a.Re,a.Im with
        |x,y when x<0 && y>0 -> phase + Math.PI 
        |x,y when x<0 && y<0 -> phase - Math.PI 
        |_ -> phase
    {Mag=mag;Phase=phase'}

let complexPToC (a:ComplexP)=
    let x = a.Mag * (cos a.Phase)
    let y = a.Mag * (sin a.Phase)
    {Re=x;Im=y}

let complexDiv (a:ComplexC) (b:ComplexC) : ComplexC =
    let a',b' = toComplexJS a, toComplexJS b
    let div = Maths.divide (a', b')
    {Re= (Maths.re div); Im= Maths.im div}


/// Converts the flattened matrix to an nxn matrix in the MathJS environment (obj)
let flattenedToMatrix flat=
    let flattenedMatrix =
        flat
        |> Array.map (toComplexJS)

    let isPerfectSquare no = box no :? int          

    let perfectSquare = isPerfectSquare (flattenedMatrix |> Array.length |> float |> sqrt) 
    match perfectSquare with
    |false -> failwithf "Wrong array size -> cannot convert to Matrix"
    |true ->
        let dim = flattenedMatrix |> Array.length |> float |> sqrt |> int
        Maths.reshape (ResizeArray(flattenedMatrix), ResizeArray([|dim; dim|]))


/// Scales the matrix accordingly so that the conductance matrix G
/// has mean 1. This is used to check whether det is zero since
/// MathJS det is not completely accurate and can return 0 for a 
/// small non-zero det
let transformMatrixToMeanOne (nodesNo:int) (flattenedMatrix:ComplexC array) =
    let len = Array.length flattenedMatrix
    let sqrt = len |> float |> sqrt |> int
    let isInCondMatrix (index:int) =
        if index < (len/nodesNo) then
            if index % sqrt < nodesNo then true
            else false
        else false

    let indexed = flattenedMatrix |> Array.indexed 
    let mean=
        (0.0,indexed)
        ||> Array.fold (fun s (i,v) -> 
            let polar = complexCToP v
            if isInCondMatrix i then s+polar.Mag else s)

    //printfn "mean = %f" mean
    flattenedMatrix
    |> Array.mapi (fun i v -> 
        v*(1./mean)
    )

/// Given MNA matrix and vecB solves the system
/// by inverting the matrix and multiplying vecB
/// Returns None if det = 0 -> matrix not invertible
let safeSolveMatrixVec flattenedMatrix vec nodesNo =
    
    let matrixMeanOne =  
        flattenedMatrix
        |> transformMatrixToMeanOne nodesNo
        |> Array.map (fun c -> {c with Im = 0.})
        |> flattenedToMatrix

    let matrix =  
        flattenedMatrix
        |> Array.map (fun c -> {c with Im = 0.})
        |> flattenedToMatrix 

    let det = Maths.det(matrixMeanOne)
    //printfn "det %s" (string det)


    if Maths.isZero det || (string det) = "NaN" then
        None
    else
        let dim = flattenedMatrix |> Array.length |> float |> sqrt |> int
        let invM = Maths.inv(matrix)
        match invM = null with
        |true -> 
            None
        |false ->
        //printfn "inv %A" invM
            match dim = Array.length vec with
            |false -> failwithf "Cannot perform multiplication -> sizes do not match"
            |true -> 
                let res = Maths.multiply (invM,ResizeArray(vec))
                res.ToArray() |> Array.map (Maths.re) |> Some




let safeInvComplexMatrix (flattenedMatrix:ComplexC array) =
    let matrix = 
        flattenedToMatrix flattenedMatrix

    
    let det = Maths.det(matrix)

    match det = 0.0 with
    |true -> failwithf "det is 0, cannot invert"
    |false ->
        let invM = Maths.inv(matrix)
        let flat = Maths.reshape (invM, ResizeArray([|Array.length flattenedMatrix; 1|])) 

        flat.ToArray()
        |> Array.map (toComplexF)


/// Solves Circuit for AC Analysis (matrix and sols are complex)
/// No need to check for det as it is already checked when starting
/// the simulation
let safeSolveMatrixVecComplex flattenedMatrix vec nodesNo =
    let matrix =  
        flattenedMatrix
        |> flattenedToMatrix
    
    
    let jsVec = vec |> Array.map(toComplexJS)

    let dim = flattenedMatrix |> Array.length |> float |> sqrt |> int
    let invM = Maths.inv(matrix)
    match dim = Array.length vec with
    |false -> failwithf "Cannot perform multiplication -> sizes do not match"
    |true -> 
        let res = Maths.multiply (invM,jsVec)
        res.ToArray() 
        |> Array.map (toComplexF)
            