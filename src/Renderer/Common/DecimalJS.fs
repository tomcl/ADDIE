module rec DecimalJS

#nowarn "3390" // disable warnings for invalid XML comments
#nowarn "0044" // disable warnings for `Obsolete` usage

open System
open Fable.Core
open Fable.Core.JS

[<Erase>] type KeyOf<'T> = Key of string
type Array<'T> = System.Collections.Generic.IList<'T>
type RegExp = System.Text.RegularExpressions.Regex
type Record<'T,'S> = 'T * 'S
let [<ImportAll("module")>] decimal: Decimal.IExports = jsNative

type [<AllowNullLiteral>] IExports =
    abstract Decimal: DecimalStatic

module Decimal =

    type Constructor =
        obj

    type Instance =
        Decimal

    type [<RequireQualifiedAccess>] Rounding =
        | N0 = 0
        | N1 = 1
        | N2 = 2
        | N3 = 3
        | N4 = 4
        | N5 = 5
        | N6 = 6
        | N7 = 7
        | N8 = 8

    type Modulo =
        U2<Rounding, float>

    type Value =
        U3<string, float, Decimal>

    type [<AllowNullLiteral>] Config =
        abstract precision: float option with get, set
        abstract rounding: Rounding option with get, set
        abstract toExpNeg: float option with get, set
        abstract toExpPos: float option with get, set
        abstract minE: float option with get, set
        abstract maxE: float option with get, set
        abstract crypto: bool option with get, set
        abstract modulo: Modulo option with get, set
        abstract defaults: bool option with get, set

type [<AllowNullLiteral>] Decimal =
    abstract d: ResizeArray<float>
    abstract e: float
    abstract s: float
    abstract absoluteValue: unit -> Decimal
    abstract abs: unit -> Decimal
    abstract ceil: unit -> Decimal
    abstract clampedTo: min: Decimal.Value * max: Decimal.Value -> Decimal
    abstract clamp: min: Decimal.Value * max: Decimal.Value -> Decimal
    abstract comparedTo: n: Decimal.Value -> float
    abstract cmp: n: Decimal.Value -> float
    abstract cosine: unit -> Decimal
    abstract cos: unit -> Decimal
    abstract cubeRoot: unit -> Decimal
    abstract cbrt: unit -> Decimal
    abstract decimalPlaces: unit -> float
    abstract dp: unit -> float
    abstract dividedBy: n: Decimal.Value -> Decimal
    abstract div: n: Decimal.Value -> Decimal
    abstract dividedToIntegerBy: n: Decimal.Value -> Decimal
    abstract divToInt: n: Decimal.Value -> Decimal
    abstract equals: n: Decimal.Value -> bool
    abstract eq: n: Decimal.Value -> bool
    abstract floor: unit -> Decimal
    abstract greaterThan: n: Decimal.Value -> bool
    abstract gt: n: Decimal.Value -> bool
    abstract greaterThanOrEqualTo: n: Decimal.Value -> bool
    abstract gte: n: Decimal.Value -> bool
    abstract hyperbolicCosine: unit -> Decimal
    abstract cosh: unit -> Decimal
    abstract hyperbolicSine: unit -> Decimal
    abstract sinh: unit -> Decimal
    abstract hyperbolicTangent: unit -> Decimal
    abstract tanh: unit -> Decimal
    abstract inverseCosine: unit -> Decimal
    abstract acos: unit -> Decimal
    abstract inverseHyperbolicCosine: unit -> Decimal
    abstract acosh: unit -> Decimal
    abstract inverseHyperbolicSine: unit -> Decimal
    abstract asinh: unit -> Decimal
    abstract inverseHyperbolicTangent: unit -> Decimal
    abstract atanh: unit -> Decimal
    abstract inverseSine: unit -> Decimal
    abstract asin: unit -> Decimal
    abstract inverseTangent: unit -> Decimal
    abstract atan: unit -> Decimal
    abstract isFinite: unit -> bool
    abstract isInteger: unit -> bool
    abstract isInt: unit -> bool
    abstract isNaN: unit -> bool
    abstract isNegative: unit -> bool
    abstract isNeg: unit -> bool
    abstract isPositive: unit -> bool
    abstract isPos: unit -> bool
    abstract isZero: unit -> bool
    abstract lessThan: n: Decimal.Value -> bool
    abstract lt: n: Decimal.Value -> bool
    abstract lessThanOrEqualTo: n: Decimal.Value -> bool
    abstract lte: n: Decimal.Value -> bool
    abstract logarithm: ?n: Decimal.Value -> Decimal
    abstract log: ?n: Decimal.Value -> Decimal
    abstract minus: n: Decimal.Value -> Decimal
    abstract sub: n: Decimal.Value -> Decimal
    abstract modulo: n: Decimal.Value -> Decimal
    abstract ``mod``: n: Decimal.Value -> Decimal
    abstract naturalExponential: unit -> Decimal
    abstract exp: unit -> Decimal
    abstract naturalLogarithm: unit -> Decimal
    abstract ln: unit -> Decimal
    abstract negated: unit -> Decimal
    abstract neg: unit -> Decimal
    abstract plus: n: Decimal.Value -> Decimal
    abstract add: n: Decimal.Value -> Decimal
    abstract precision: ?includeZeros: bool -> float
    abstract sd: ?includeZeros: bool -> float
    abstract round: unit -> Decimal
    abstract sine: unit -> Decimal
    abstract sin: unit -> Decimal
    abstract squareRoot: unit -> Decimal
    abstract sqrt: unit -> Decimal
    abstract tangent: unit -> Decimal
    abstract tan: unit -> Decimal
    abstract times: n: Decimal.Value -> Decimal
    abstract mul: n: Decimal.Value -> Decimal
    abstract toBinary: ?significantDigits: float -> string
    abstract toBinary: significantDigits: float * rounding: Decimal.Rounding -> string
    abstract toDecimalPlaces: ?decimalPlaces: float -> Decimal
    abstract toDecimalPlaces: decimalPlaces: float * rounding: Decimal.Rounding -> Decimal
    abstract toDP: ?decimalPlaces: float -> Decimal
    abstract toDP: decimalPlaces: float * rounding: Decimal.Rounding -> Decimal
    abstract toExponential: ?decimalPlaces: float -> string
    abstract toExponential: decimalPlaces: float * rounding: Decimal.Rounding -> string
    abstract toFixed: ?decimalPlaces: float -> string
    abstract toFixed: decimalPlaces: float * rounding: Decimal.Rounding -> string
    abstract toFraction: ?max_denominator: Decimal.Value -> ResizeArray<Decimal>
    abstract toHexadecimal: ?significantDigits: float -> string
    abstract toHexadecimal: significantDigits: float * rounding: Decimal.Rounding -> string
    abstract toHex: ?significantDigits: float -> string
    abstract toHex: significantDigits: float * ?rounding: Decimal.Rounding -> string
    abstract toJSON: unit -> string
    abstract toNearest: n: Decimal.Value * ?rounding: Decimal.Rounding -> Decimal
    abstract toNumber: unit -> float
    abstract toOctal: ?significantDigits: float -> string
    abstract toOctal: significantDigits: float * rounding: Decimal.Rounding -> string
    abstract toPower: n: Decimal.Value -> Decimal
    abstract pow: n: Decimal.Value -> Decimal
    abstract toPrecision: ?significantDigits: float -> string
    abstract toPrecision: significantDigits: float * rounding: Decimal.Rounding -> string
    abstract toSignificantDigits: ?significantDigits: float -> Decimal
    abstract toSignificantDigits: significantDigits: float * rounding: Decimal.Rounding -> Decimal
    abstract toSD: ?significantDigits: float -> Decimal
    abstract toSD: significantDigits: float * rounding: Decimal.Rounding -> Decimal
    abstract toString: unit -> string
    abstract truncated: unit -> Decimal
    abstract trunc: unit -> Decimal
    abstract valueOf: unit -> string

type [<AllowNullLiteral>] DecimalStatic =
    [<EmitConstructor>] abstract Create: n: Decimal.Value -> Decimal
    abstract abs: n: Decimal.Value -> Decimal
    abstract acos: n: Decimal.Value -> Decimal
    abstract acosh: n: Decimal.Value -> Decimal
    abstract add: x: Decimal.Value * y: Decimal.Value -> Decimal
    abstract asin: n: Decimal.Value -> Decimal
    abstract asinh: n: Decimal.Value -> Decimal
    abstract atan: n: Decimal.Value -> Decimal
    abstract atanh: n: Decimal.Value -> Decimal
    abstract atan2: y: Decimal.Value * x: Decimal.Value -> Decimal
    abstract cbrt: n: Decimal.Value -> Decimal
    abstract ceil: n: Decimal.Value -> Decimal
    abstract clamp: n: Decimal.Value * min: Decimal.Value * max: Decimal.Value -> Decimal
    abstract clone: ?object: Decimal.Config -> Decimal.Constructor
    abstract config: object: Decimal.Config -> Decimal.Constructor
    abstract cos: n: Decimal.Value -> Decimal
    abstract cosh: n: Decimal.Value -> Decimal
    abstract div: x: Decimal.Value * y: Decimal.Value -> Decimal
    abstract exp: n: Decimal.Value -> Decimal
    abstract floor: n: Decimal.Value -> Decimal
    abstract hypot: [<ParamArray>] n: Decimal.Value[] -> Decimal
    abstract isDecimal: object: obj option -> bool
    abstract ln: n: Decimal.Value -> Decimal
    abstract log: n: Decimal.Value * ?``base``: Decimal.Value -> Decimal
    abstract log2: n: Decimal.Value -> Decimal
    abstract log10: n: Decimal.Value -> Decimal
    abstract max: [<ParamArray>] n: Decimal.Value[] -> Decimal
    abstract min: [<ParamArray>] n: Decimal.Value[] -> Decimal
    abstract ``mod``: x: Decimal.Value * y: Decimal.Value -> Decimal
    abstract mul: x: Decimal.Value * y: Decimal.Value -> Decimal
    abstract noConflict: unit -> Decimal.Constructor
    abstract pow: ``base``: Decimal.Value * exponent: Decimal.Value -> Decimal
    abstract random: ?significantDigits: float -> Decimal
    abstract round: n: Decimal.Value -> Decimal
    abstract set: object: Decimal.Config -> Decimal.Constructor
    abstract sign: n: Decimal.Value -> float
    abstract sin: n: Decimal.Value -> Decimal
    abstract sinh: n: Decimal.Value -> Decimal
    abstract sqrt: n: Decimal.Value -> Decimal
    abstract sub: x: Decimal.Value * y: Decimal.Value -> Decimal
    abstract sum: [<ParamArray>] n: Decimal.Value[] -> Decimal
    abstract tan: n: Decimal.Value -> Decimal
    abstract tanh: n: Decimal.Value -> Decimal
    abstract trunc: n: Decimal.Value -> Decimal
    abstract ``default``: Decimal.Constructor option
    abstract Decimal: Decimal.Constructor option
    abstract precision: float
    abstract rounding: Decimal.Rounding
    abstract toExpNeg: float
    abstract toExpPos: float
    abstract minE: float
    abstract maxE: float
    abstract crypto: bool
    abstract modulo: Decimal.Modulo
    abstract ROUND_UP: int
    abstract ROUND_DOWN: int
    abstract ROUND_CEIL: int
    abstract ROUND_FLOOR: int
    abstract ROUND_HALF_UP: int
    abstract ROUND_HALF_DOWN: int
    abstract ROUND_HALF_EVEN: int
    abstract ROUND_HALF_CEIL: int
    abstract ROUND_HALF_FLOOR: int
    abstract EUCLID: int
