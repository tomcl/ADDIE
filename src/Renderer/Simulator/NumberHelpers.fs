(*
    NumberHelpers.fs

    A collection of functions that allow to covert numbers into various
    representations.
*)

module NumberHelpers
open CommonTypes
open Helpers

/// Convert an hex string into a binary string.
let private hexToBin (hStr : string) : string =
    let rec convert h =
        match h with
        | [] -> ""
        | c :: h' ->
            let digit =
                match c with
                | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011"
                | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
                | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
                | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
                | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr
            digit + (convert h')
    let chars = hStr.ToLower() |> Seq.toList
    match chars with
    | [] -> ""
    | c :: chars' ->
        let firstDigit = // Avoid leading zeros.
            match c with
            | '0' -> "0" | '1' -> "1" | '2' -> "10" | '3' -> "11"
            | '4' -> "100" | '5' -> "101" | '6' -> "110" | '7' -> "111"
            | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
            | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
            | c -> failwithf "Invalid char %c while converting hex %s to binary" c hStr
        firstDigit + (convert chars')

let addZeros64 (width:int) (pFun:int64 -> string) (n: int64) =
    let s = pFun n
    let bits = 
        match s[1] with
        | 'x' -> 4
        | 'b' -> 1
        | _ -> failwithf "Wrong use of addZeros64: s = %s" s
    let extra = min 64 (max 0 ((width - (s.Length - 2)*bits) / bits))
    s[0..1] + String.replicate extra "0" + s[2..]

let addZeros (width:int) (pFun:int -> string) (n: int) =
    let s = pFun n
    let bits = 
        match s[1] with
        | 'x' -> 4
        | 'b' -> 1
        | _ -> failwithf "Wrong use of addZeros: s = %s" s
    let extra = min 64 (max 0 (((width - (s.Length - 2))*bits + (2<<<bits - 1)) / bits))
    s[0..1] + String.replicate extra "0" + s[2..]

let hex64 (num : int64) = "0x" + num.ToString("X")
let fillHex64 width = addZeros64 width hex64

let bin64 (num : int64) = "0b" + (hexToBin <| num.ToString("X"))
let sDec64 (num : int64) = num.ToString()
let dec64 (num: int64) = (uint64 num).ToString()

let hex (num : int) = hex64 <| int64 num
let fillHex width = addZeros width hex

let bin (num : int) = bin64 <| int64 num
let dec (num : int) = dec64 <| int64 num

let fillBin64 width = addZeros64 width bin64
let fillBin width = addZeros width bin


/// Convert int64 to string according to provided radix
let valToString (radix: NumberBase) (value: int64) : string =
    match radix with
    | Dec -> dec64 value
    | Bin -> bin64 value
    | Hex -> hex64 value
    | SDec -> sDec64 value

/// Convert int64 to string according to radix.
/// binary and hex numbers are zero padded to width
/// binary is displayed as hex if width > 8
let valToPaddedString (width: int) (radix: NumberBase) (value: int64) : string =
    match radix with
    | Dec -> dec64 value
    | Bin when width <= 8 -> fillBin64 width value
    | Hex | Bin -> fillHex64 width value
    | SDec -> sDec64 value





/// Try to convert a string to an int, or return an error message if that was
/// not possible.
let strToInt (str : string) : Result<int64, string> =
    try
        Ok <| int64 str
    with
        | _ -> Error <| "Invalid number."

        (*

let toInt = EEExtensions.Char.toInt

/// convert a digit character: binary, decimal, or hexadecimal, to its numeric value
let cDigitInt (ch:char) =
    match toInt ch with
    | d when d >= int32 '0' && d <= int32 '9' -> Some(d - int32 '0')
    | d when d >= toInt 'A' && d <= toInt 'Z' -> Some(d - toInt 'A' + 10)
    | _ -> None

let convertUInt64 (stringToConvert: string) =
    let rec pow64 n = 
    let getRadixNum (radix:int) (ns: int option list) =
        if Seq.forall (function | Some n -> n < radix && n >= 0 | None -> false) ns
        then Some (List.sumBy (fun n -> uint64 n + ))

    let aInt = toInt 'A'
    let s = EEExtensions.String.trim (EEExtensions.String.toUpper stringToConvert)
    if EEExtensions.String.startsWith "0X" s then
        let hexDigits = s[2..s.Length-1]
        let convDigits = hexDigits |> List.map cDigitInt 
        if checkRadix 16

*)

let private countBits (num : int64) : int =
    (String.length <| bin64 num) - 2

/// Check a number is formed by at most <width> bits.
let rec checkWidth (width : int) (num : int64) : string option =
    if num < 0L then
        checkWidth width <| (-num) - 1L
    else    
        let bitsCount = countBits num
        match bitsCount <= width with
        | true -> None
        | false -> Some <| sprintf "Expected %d or less bits." width

/// Convert a string to a number making sure that it has no more bits than
/// specified in width.
let strToIntCheckWidth (width : int) (str : string)  : Result<int64, string> =
    match str.Trim() with
    | "" -> Ok 0L // special case
    | str ->
        strToInt str
        |> Result.bind (fun num ->
            match checkWidth width num with
            | None -> Ok num
            | Some err -> Error err
        )

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
                    | 'K' | 'k' -> 1000.* (float beginning) |> Some
                    | 'M' -> 1000000.* (float beginning)|> Some
                    | 'm' -> 0.001* (float beginning)|> Some
                    | 'u' -> 0.000001* (float beginning)|> Some
                    | 'n' -> 0.000000001* (float beginning)|> Some
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
                    |y when y[0..2] = "000" -> y[3..(Seq.length floatPart-1)]+" u"
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



