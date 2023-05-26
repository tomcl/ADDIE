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
    abstract det : obj -> float
    abstract re: obj -> float
    abstract im: obj -> float
    abstract rationalize: string -> string
    //abstract atan: float -> float

[<ImportAll("mathjs")>]
let Maths: MathsJS = jsNative




let toComplexJS (a:ComplexC) =
    (a.Re,a.Im) |> Maths.complex

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

let safeSolveMatrixVec flattenedMatrix vec =
    let matrix =  
        flattenedMatrix
        |> Array.map (fun c -> {c with Im = 0.})
        |> flattenedToMatrix
    let det = Maths.det(matrix)
    
    if string det = "0" then
        None
        //failwithf "det is 0, cannot invert"
    else
        let dim = flattenedMatrix |> Array.length |> float |> sqrt |> int
        let invM = Maths.inv(matrix)
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

let safeSolveMatrixVecComplex flattenedMatrix vec =
    let matrix =  
        flattenedMatrix
        |> flattenedToMatrix
    let det = Maths.det(matrix)

    let jsVec = vec |> Array.map(toComplexJS)

    match det = 0.0 with
    |true -> failwithf "det is 0, cannot invert"
    |false ->
        let dim = flattenedMatrix |> Array.length |> float |> sqrt |> int
        let invM = Maths.inv(matrix)
        match dim = Array.length vec with
        |false -> failwithf "Cannot perform multiplication -> sizes do not match"
        |true -> 
            let res = Maths.multiply (invM,jsVec)
            res.ToArray() 
            |> Array.map (toComplexF)
