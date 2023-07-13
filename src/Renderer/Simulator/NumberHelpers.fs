(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers
open CommonTypes

module Constants =
    let displayPrecision = 3

/// Return a display string for a float q
/// displayed to given precision using the appropriate SI multiplier.
/// Reverts to scientific notation if abs value of number is too large.
/// Displays 0 if abs value of number is too small.
let rec displayWithSIMultiplier (precision: int) (q: float): string =
    if q < 0.0 then 
        // deal with negative inputs
        "-" + displayWithSIMultiplier precision -q
    else
        let multipliers = [
            9, "G"
            6, "M"
            3, "k"
            0, ""
            -3, "m"
            -6, "u"
            -9, "n"
            -12, "p"
            ]

        let displayWithPrecision : (float -> string) =
            match precision with
            | 1 -> sprintf "%.1g"
            | 2 -> sprintf "%.2g"
            | 3 -> sprintf "%.3g"
            | 4 -> sprintf "%.4g"
            | 5 -> sprintf "%.5g"
            | _ -> sprintf "%.6g"

        let tryGetDisplay (unitExp:int,unitName:string) =
            let scaledQty = (q / 10.0 ** float unitExp)
            match scaledQty > 10000.0, int scaledQty with
            | true, _ ->
                Some <| displayWithPrecision q
            | false, 0 -> 
                // too small, try another unit
                None 
            | _ ->
                // The right size
                Some <| displayWithPrecision scaledQty + unitName
        multipliers
        |> List.tryPick tryGetDisplay
        |> Option.defaultValue "0"


/// Converts the text input of an RLCI Popup to its float value (Option)
/// Returns None in case the input has invalid format
let textToFloatValue (text:string) =
    let isDigitOrDot (c:char) =
        (c = '.' || System.Char.IsDigit c)

    let checkNoChars (s:string) =
        s |> Seq.forall isDigitOrDot
    
    match String.length text with
    |0 -> Some 0.
    |length ->
        match text |> Seq.last with
        | ch when  System.Char.IsNumber ch -> if checkNoChars text then (float text) |> Some else None
        | last ->
            let beginning = text.Remove (length-1)
            let bLength = String.length beginning 
            match checkNoChars beginning with
            |true ->
                if String.length (beginning.TrimEnd [|'.'|]) = bLength 
                    || String.length (beginning.TrimEnd [|'.'|]) = (bLength-1)
                then
                    match last with
                    | 'K' | 'k' -> 1e3 * (float beginning) |> Some
                    | 'M' -> 1e6 * (float beginning)|> Some
                    | 'm' -> 1e-3 * (float beginning)|> Some
                    | 'u' -> 1e-6 * (float beginning)|> Some
                    | 'n' -> 1e-9 * (float beginning)|> Some
                    | "p" -> 1e-12 * float beginning |> Some
                    | "R" -> float beginning |> Some
                    | _ -> None
                else None
            |false -> None 



let floatValueToText (value:float) :string =
    let asStr = string value
    let hasDot = asStr |> Seq.contains '.'
    let hasMinus = asStr[0]='-'
    
    let str = if hasMinus then asStr[1..(String.length asStr)] else asStr
    let length = Seq.length str   

    let result=
        match hasDot with
        |true ->
            let dotIndex = str |> Seq.findIndex (fun ch -> ch='.')
            let intPart = str[0..(dotIndex-1)]//str[dotIndex..(Seq.length str-dotIndex)]
            let floatPart = str[(dotIndex+1)..(String.length str-1)]//str |> Seq.removeManyAt 0 (dotIndex+1) |> string
            // case <1
            if intPart = "0" then
                let cut = 
                    match floatPart with
                    |x when x[0..5] = "000000" -> x[6..(Seq.length floatPart-1)]+" n"
                    |y when y[0..2] = "000" -> y[3..(Seq.length floatPart-1)]+" "+muString
                    |_ -> floatPart+" m"
                let cut' =
                    if String.length cut > 5 then cut[0..2]+"."+cut[3..(String.length cut-1)] 
                    else if String.length cut = 5 then cut
                    else if String.length cut = 4 then cut[0..1]+"0"+cut[2..3]
                    else (string cut[0])+"00"+cut[1..2]
                ("",cut') ||> Seq.fold (fun s v -> if (s="" && v='0') then s else s+(string v))
            else str
        |false ->
            match str with
            |x when x[(length-6)..(length-1)] = "000000" -> x[0..(length-7)]+" M"
            |y when y[(length-3)..(length-1)] = "000" -> y[0..(length-4)]+" k"
            |_ -> str+" "
    
    match hasMinus with
    |true -> "-"+result
    |false -> result



