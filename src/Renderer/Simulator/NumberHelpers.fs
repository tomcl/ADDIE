(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers
open CommonTypes
open EEExtensions

module Constants =
    let displayPrecision = 3
    let sIMultipliers = [
            9, "G"
            6, "M"
            3, "k"
            0, ""
            -3, "m"
            -6, "u"
            -9, "n"
            -12, "p"
            ]
    let validSISuffixes = 
        sIMultipliers 
        |> List.map snd 
        |> String.concat ""

/// Return a display string for a float q
/// displayed to given precision using the appropriate SI multiplier.
/// Reverts to scientific notation if abs value of number is too large.
/// Displays 0 if abs value of number is too small.
let rec displayWithSIMultiplier (precision: int) (q: float): string =
    if q < 0.0 then 
        // deal with negative inputs
        "-" + displayWithSIMultiplier precision -q
    else

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
        Constants.sIMultipliers
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
    | 0 -> Some 0.0
    | length ->
        match System.Double.TryParse(text, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture) with
        | true, result -> Some result
        | _ ->
            let beginning = text.Remove (length-1)
            let bLength = String.length beginning 
            match checkNoChars beginning with
            | true ->
                if String.length (beginning.TrimEnd [|'.'|]) = bLength || String.length (beginning.TrimEnd [|'.'|]) = (bLength-1) then
                    match text.[length-1] with
                    | 'K' | 'k' -> Some (1e3 * (float beginning))
                    | 'M' -> Some (1e6 * (float beginning))
                    | 'm' -> Some (1e-3 * (float beginning))
                    | 'u' -> Some (1e-6 * (float beginning))
                    | 'n' -> Some (1e-9 * (float beginning))
                    | 'p' -> Some (1e-12 * (float beginning))
                    | 'R' -> Some (float beginning)
                    | _ -> None
                else None
            | false -> None



/// Converts the text in an RCLI Popup to an  float Result value.
/// In case of R or C or V or I or L  popups R/F/V/A/H is a valid optional suffix (the unit).
/// The valid suffix for a given call is provided in popupUnitSuffix.
/// The error string is a helpful (it is hoped) error message.
let popupTextToFloat (popupUnitSuffix: string option) (text:string) =
    let removeSuffixOpt suffix text =
        if String.endsWith suffix text then
            Some text[0..text.Length - suffix.Length - 1 ]
        else None

    let removeSuffix suffix text =
        removeSuffixOpt suffix text
        |> Option.defaultValue text
    let text' = 
        text
        |> removeSuffix (Option.defaultValue "" popupUnitSuffix)

    let digits, siUnit = // splits into a number and all terminating alpha chars
        String.regexMatchGroups "([/d.]*)([a-zA-Z]*)$" text' 
        |> Option.map (fun grps -> grps[0], grps[1])
        |> Option.defaultValue (text',"")

    let knownBadUnitMsg = 
        match siUnit with
        | "R" -> Some "a resistor"
        | "H" -> Some "an inductor"
        | "F" -> Some "a capacitor"
        | "V" -> Some "a voltage source"
        | "A" -> Some "a current source"
        | _ -> None 
        |> Option.map (fun unitName -> Error <| siUnit +  "is not valid here - this popup is not " + unitName)

    let multiplier =
        Constants.sIMultipliers
        |> List.tryPick (fun (exp, name) -> if name = siUnit then Some (Ok (10.0**float exp)) else None)
        |> Option.defaultValue (Error $"{siUnit} is not a valid SI unit multiplier - did you mean one of {Constants.validSISuffixes}?")
      
    match knownBadUnitMsg, String.tryParseWith System.Double.TryParse digits, multiplier with
    | Some badUnitMsg, _, _ -> badUnitMsg
    | _, _, Error multiplierMsg  -> 
        Error multiplierMsg
    | _, Some numberParse, Ok multiplier -> 
        Ok (numberParse*multiplier)
    | _, None, _ -> 
        Error $"'{digits}' is not a valid number"





let floatValueToText (value:float) :string =
    let asStr = if ((string value) |> Seq.exists (fun c -> c = 'e' || c = 'E')) then sprintf "%.10f" value else string value
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



