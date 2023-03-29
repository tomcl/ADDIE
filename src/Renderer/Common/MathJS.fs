// ts2fable 0.9.0
module rec MathJS

#nowarn "3390" // disable warnings for invalid XML comments
#nowarn "0044" // disable warnings for `Obsolete` usage

open System
open Fable.Core
open Fable.Core.JS

[<Erase>] type KeyOf<'T> = Key of string
type Array<'T> = System.Collections.Generic.IList<'T>
type RegExp = System.Text.RegularExpressions.Regex

let [<Import("math","module")>] math: Math.MathJsStatic = jsNative

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




module Math =

    type [<AllowNullLiteral>] IExports =
        abstract NodeCtor: NodeCtorStatic
        abstract AccessorNodeCtor: AccessorNodeCtorStatic
        abstract ArrayNodeCtor: ArrayNodeCtorStatic
        abstract AssignmentNodeCtor: AssignmentNodeCtorStatic
        abstract BlockNodeCtor: BlockNodeCtorStatic
        abstract ConditionalNodeCtor: ConditionalNodeCtorStatic
        abstract ConstantNodeCtor: ConstantNodeCtorStatic
        abstract FunctionAssignmentNodeCtor: FunctionAssignmentNodeCtorStatic
        abstract FunctionNodeCtor: FunctionNodeCtorStatic
        abstract IndexNodeCtor: IndexNodeCtorStatic
        abstract ObjectNodeCtor: ObjectNodeCtorStatic
        abstract OperatorNodeCtor: OperatorNodeCtorStatic
        abstract ParenthesisNodeCtor: ParenthesisNodeCtorStatic
        abstract RangeNodeCtor: RangeNodeCtorStatic
        abstract RelationalNodeCtor: RelationalNodeCtorStatic
        abstract SymbolNodeCtor: SymbolNodeCtorStatic
        abstract MatrixCtor: MatrixCtorStatic

    type MathNumericType =
        U4<float, BigNumber, Fraction, Complex>

    type MathArray =
        U2<ResizeArray<MathNumericType>, ResizeArray<ResizeArray<MathNumericType>>>

    type MathCollection =
        U2<MathArray, Matrix>

    type MathType =
        U3<MathNumericType, Unit, MathCollection>

    type MathExpression =
        U3<string, ResizeArray<string>, MathCollection>

    type [<AllowNullLiteral>] FactoryFunction<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: scope: obj option -> 'T

    type [<AllowNullLiteral>] FactoryFunctionMap =
        [<EmitIndexer>] abstract Item: key: string -> U2<FactoryFunction<obj option>, FactoryFunctionMap> with get, set

    /// Available options for parse
    type [<AllowNullLiteral>] ParseOptions =
        /// a set of custom nodes
        abstract nodes: Record<string, MathNode> option with get, set

    /// <summary>
    /// Parse an expression. Returns a node tree, which can be evaluated by
    /// invoking node.evaluate().
    /// 
    /// Note the evaluating arbitrary expressions may involve security risks,
    /// see <see href="https://mathjs.org/docs/expressions/security.html">https://mathjs.org/docs/expressions/security.html</see> for more information.
    /// 
    /// Syntax:
    /// 
    ///     math.parse(expr)
    ///     math.parse(expr, options)
    ///     math.parse([expr1, expr2, expr3, ...])
    ///     math.parse([expr1, expr2, expr3, ...], options)
    /// 
    /// Example:
    /// 
    ///     const node1 = math.parse('sqrt(3^2 + 4^2)')
    ///     node1.compile().evaluate() // 5
    /// 
    ///     let scope = {a:3, b:4}
    ///     const node2 = math.parse('a * b') // 12
    ///     const code2 = node2.compile()
    ///     code2.evaluate(scope) // 12
    ///     scope.a = 5
    ///     code2.evaluate(scope) // 20
    /// 
    ///     const nodes = math.parse(['a = 3', 'b = 4', 'a * b'])
    ///     nodes[2].compile().evaluate() // 12
    /// 
    /// See also:
    /// 
    ///     evaluate, compile
    /// </summary>
    type [<AllowNullLiteral>] ParseFunction =
        /// <summary>
        /// Parse an expression. Returns a node tree, which can be evaluated by
        /// invoking node.evaluate();
        /// </summary>
        /// <param name="expr">Expression to be parsed</param>
        /// <param name="options">Available options</param>
        /// <returns>A node</returns>
        [<Emit("$0($1...)")>] abstract Invoke: expr: MathExpression * ?options: ParseOptions -> MathNode
        /// <summary>
        /// Parse an expression. Returns a node tree, which can be evaluated by
        /// invoking node.evaluate();
        /// </summary>
        /// <param name="exprs">Expressions to be parsed</param>
        /// <param name="options">Available options</param>
        /// <returns>An array of nodes</returns>
        [<Emit("$0($1...)")>] abstract Invoke: exprs: ResizeArray<MathExpression> * ?options: ParseOptions -> ResizeArray<MathNode>
        /// <summary>
        /// Checks whether the current character <c>c</c> is a valid alpha character:
        /// 
        /// - A latin letter (upper or lower case) Ascii: a-z, A-Z
        /// - An underscore                        Ascii: _
        /// - A dollar sign                        Ascii: $
        /// - A latin letter with accents          Unicode: \u00C0 - \u02AF
        /// - A greek letter                       Unicode: \u0370 - \u03FF
        /// - A mathematical alphanumeric symbol   Unicode: \u{1D400} - \u{1D7FF} excluding invalid code points
        /// 
        /// The previous and next characters are needed to determine whether
        /// this character is part of a unicode surrogate pair.
        /// </summary>
        /// <param name="c">Current character in the expression</param>
        /// <param name="cPrev">Previous character</param>
        /// <param name="cNext">Next character</param>
        abstract isAlpha: c: string * cPrev: string * cNext: string -> bool
        /// <summary>Test whether a character is a valid latin, greek, or letter-like character</summary>
        /// <param name="c" />
        abstract isValidLatinOrGreek: c: string -> bool
        /// <summary>
        /// Test whether two given 16 bit characters form a surrogate pair of a
        /// unicode math symbol.
        /// 
        /// <see href="https://unicode-table.com/en/" />
        /// <see href="https://www.wikiwand.com/en/Mathematical_operators_and_symbols_in_Unicode" />
        /// 
        /// Note: In ES6 will be unicode aware:
        /// <see href="https://stackoverflow.com/questions/280712/javascript-unicode-regexes" />
        /// <see href="https://mathiasbynens.be/notes/es6-unicode-regex" />
        /// </summary>
        /// <param name="high" />
        /// <param name="low" />
        abstract isValidMathSymbol: high: string * low: string -> bool
        /// <summary>Check whether given character c is a white space character: space, tab, or enter</summary>
        /// <param name="c" />
        /// <param name="nestingLevel" />
        abstract isWhitespace: c: string * nestingLevel: float -> bool
        /// <summary>
        /// Test whether the character c is a decimal mark (dot).
        /// This is the case when it's not the start of a delimiter '.*', './', or '.^'
        /// </summary>
        /// <param name="c" />
        /// <param name="cNext" />
        abstract isDecimalMark: c: string * cNext: string -> bool
        /// <summary>checks if the given char c is a digit or dot</summary>
        /// <param name="c">a string with one character</param>
        abstract isDigitDot: c: string -> bool
        /// <summary>checks if the given char c is a digit</summary>
        /// <param name="c">a string with one character</param>
        abstract isDigit: c: string -> bool
        /// <summary>checks if the given char c is a hex digit</summary>
        /// <param name="c">a string with one character</param>
        abstract isHexDigit: c: string -> bool

    type [<AllowNullLiteral>] NodeCtor =
        interface end

    type [<AllowNullLiteral>] NodeCtorStatic =
        [<EmitConstructor>] abstract Create: unit -> NodeCtor

    type AccessorNode =
        AccessorNode<MathNode>

    type [<AllowNullLiteral>] AccessorNode<'TObject when 'TObject :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isAccessorNode: bool with get, set
        abstract object: 'TObject with get, set
        abstract index: IndexNode with get, set
        abstract name: string with get, set

    type [<AllowNullLiteral>] AccessorNodeCtor =
        interface end

    type [<AllowNullLiteral>] AccessorNodeCtorStatic =
        [<EmitConstructor>] abstract Create: object: 'TObject * index: IndexNode -> AccessorNodeCtor when 'TObject :> MathNode

    type ArrayNode =
        ArrayNode<ResizeArray<MathNode>>

    type [<AllowNullLiteral>] ArrayNode<'TItems> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isArrayNode: bool with get, set
        abstract items: 'TItems with get, set

    type [<AllowNullLiteral>] ArrayNodeCtor =
        interface end

    type [<AllowNullLiteral>] ArrayNodeCtorStatic =
        [<EmitConstructor>] abstract Create: items: ResizeArray<MathNode> -> ArrayNodeCtor

    type AssignmentNode =
        AssignmentNode<MathNode>

    type [<AllowNullLiteral>] AssignmentNode<'TValue when 'TValue :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isAssignmentNode: bool with get, set
        abstract object: U2<SymbolNode, AccessorNode> with get, set
        abstract index: IndexNode option with get, set
        abstract value: 'TValue with get, set
        abstract name: string with get, set

    type [<AllowNullLiteral>] AssignmentNodeCtor =
        interface end

    type [<AllowNullLiteral>] AssignmentNodeCtorStatic =
        [<EmitConstructor>] abstract Create: object: SymbolNode * value: 'TValue -> AssignmentNodeCtor when 'TValue :> MathNode
        [<EmitConstructor>] abstract Create: object: U2<SymbolNode, AccessorNode> * index: IndexNode * value: 'TValue -> AssignmentNodeCtor when 'TValue :> MathNode

    type BlockNode =
        BlockNode<MathNode>

    type [<AllowNullLiteral>] BlockNode<'TNode when 'TNode :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isBlockNode: bool with get, set
        abstract blocks: Array<{| node: 'TNode; visible: bool |}> with get, set

    type [<AllowNullLiteral>] BlockNodeCtor =
        interface end

    type [<AllowNullLiteral>] BlockNodeCtorStatic =
        [<EmitConstructor>] abstract Create: arr: Array<U2<{| node: 'TNode |}, {| node: 'TNode; visible: bool |}>> -> BlockNodeCtor when 'TNode :> MathNode

    type ConditionalNode =
        ConditionalNode<MathNode, MathNode, MathNode>

    type ConditionalNode<'TCond when 'TCond :> MathNode> =
        ConditionalNode<'TCond, MathNode, MathNode>

    type ConditionalNode<'TCond, 'TTrueNode when 'TCond :> MathNode and 'TTrueNode :> MathNode> =
        ConditionalNode<'TCond, 'TTrueNode, MathNode>

    type [<AllowNullLiteral>] ConditionalNode<'TCond, 'TTrueNode, 'TFalseNode when 'TCond :> MathNode and 'TTrueNode :> MathNode and 'TFalseNode :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isConditionalNode: bool with get, set
        abstract condition: 'TCond with get, set
        abstract trueExpr: 'TTrueNode with get, set
        abstract falseExpr: 'TFalseNode with get, set

    type [<AllowNullLiteral>] ConditionalNodeCtor =
        interface end

    type [<AllowNullLiteral>] ConditionalNodeCtorStatic =
        [<EmitConstructor>] abstract Create: condition: 'TCond * trueExpr: 'TTrueNode * falseExpr: 'TFalseNode -> ConditionalNodeCtor when 'TCond :> MathNode and 'TTrueNode :> MathNode and 'TFalseNode :> MathNode

    type ConstantNode =
        ConstantNode<float>

    type [<AllowNullLiteral>] ConstantNode<'TValue> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isConstantNode: bool with get, set
        abstract value: 'TValue with get, set

    type [<AllowNullLiteral>] ConstantNodeCtor =
        interface end

    type [<AllowNullLiteral>] ConstantNodeCtorStatic =
        [<EmitConstructor>] abstract Create: value: 'TValue -> ConstantNodeCtor

    type FunctionAssignmentNode =
        FunctionAssignmentNode<MathNode>

    type [<AllowNullLiteral>] FunctionAssignmentNode<'TExpr when 'TExpr :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isFunctionAssignmentNode: bool with get, set
        abstract name: string with get, set
        abstract ``params``: ResizeArray<string> with get, set
        abstract expr: 'TExpr with get, set

    type [<AllowNullLiteral>] FunctionAssignmentNodeCtor =
        interface end

    type [<AllowNullLiteral>] FunctionAssignmentNodeCtorStatic =
        [<EmitConstructor>] abstract Create: name: string * ``params``: ResizeArray<string> * expr: 'TExpr -> FunctionAssignmentNodeCtor when 'TExpr :> MathNode

    type FunctionNode =
        FunctionNode<SymbolNode, ResizeArray<MathNode>>

    type FunctionNode<'TFn> =
        FunctionNode<'TFn, ResizeArray<MathNode>>

    type [<AllowNullLiteral>] FunctionNode<'TFn, 'TArgs> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isFunctionNode: bool with get, set
        abstract fn: 'TFn with get, set
        abstract args: 'TArgs with get, set

    type [<AllowNullLiteral>] FunctionNodeCtor =
        abstract onUndefinedFunction: (string -> obj option) with get, set

    type [<AllowNullLiteral>] FunctionNodeCtorStatic =
        [<EmitConstructor>] abstract Create: fn: 'TFn * args: 'TArgs -> FunctionNodeCtor

    type IndexNode =
        IndexNode<ResizeArray<MathNode>>

    type [<AllowNullLiteral>] IndexNode<'TDims> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isIndexNode: bool with get, set
        abstract dimensions: 'TDims with get, set
        abstract dotNotation: bool with get, set

    type [<AllowNullLiteral>] IndexNodeCtor =
        interface end

    type [<AllowNullLiteral>] IndexNodeCtorStatic =
        [<EmitConstructor>] abstract Create: dimensions: 'TDims -> IndexNodeCtor
        [<EmitConstructor>] abstract Create: dimensions: 'TDims * dotNotation: bool -> IndexNodeCtor

    type ObjectNode =
        ObjectNode<Record<string, MathNode>>

    type [<AllowNullLiteral>] ObjectNode<'TProps when 'TProps :> Record<string, MathNode>> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isObjectNode: bool with get, set
        abstract properties: 'TProps with get, set

    type [<AllowNullLiteral>] ObjectNodeCtor =
        interface end

    type [<AllowNullLiteral>] ObjectNodeCtorStatic =
        [<EmitConstructor>] abstract Create: properties: 'TProps -> ObjectNodeCtor when 'TProps :> Record<string, MathNode>

    type [<AllowNullLiteral>] OperatorNodeMap =
        abstract xor: string with get, set
        abstract ``and``: string with get, set
        abstract ``or``: string with get, set
        abstract bitOr: string with get, set
        abstract bitXor: string with get, set
        abstract bitAnd: string with get, set
        abstract equal: string with get, set
        abstract unequal: string with get, set
        abstract smaller: string with get, set
        abstract larger: string with get, set
        abstract smallerEq: string with get, set
        abstract largerEq: string with get, set
        abstract leftShift: string with get, set
        abstract rightArithShift: string with get, set
        abstract rightLogShift: string with get, set
        abstract ``to``: string with get, set
        abstract add: string with get, set
        abstract subtract: string with get, set
        abstract multiply: string with get, set
        abstract divide: string with get, set
        abstract dotMultiply: string with get, set
        abstract dotDivide: string with get, set
        abstract ``mod``: string with get, set
        abstract unaryPlus: string with get, set
        abstract unaryMinus: string with get, set
        abstract bitNot: string with get, set
        abstract not: string with get, set
        abstract pow: string with get, set
        abstract dotPow: string with get, set
        abstract factorial: string with get, set

    type OperatorNodeOp =
        obj

    type OperatorNodeFn =
        KeyOf<OperatorNodeMap>

    type OperatorNode =
        OperatorNode<obj, obj, ResizeArray<MathNode>>

    type OperatorNode<'TOp> =
        OperatorNode<'TOp, obj, ResizeArray<MathNode>>

    type OperatorNode<'TOp, 'TFn when 'TFn :> OperatorNodeFn> =
        OperatorNode<'TOp, 'TFn, ResizeArray<MathNode>>

    type [<AllowNullLiteral>] OperatorNode<'TOp, 'TFn, 'TArgs when 'TFn :> OperatorNodeFn> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isOperatorNode: bool with get, set
        abstract op: 'TOp with get, set
        abstract fn: 'TFn with get, set
        abstract args: 'TArgs with get, set
        abstract implicit: bool with get, set
        abstract isUnary: unit -> bool
        abstract isBinary: unit -> bool

    type [<AllowNullLiteral>] OperatorNodeCtor =
        inherit MathNode

    type [<AllowNullLiteral>] OperatorNodeCtorStatic =
        [<EmitConstructor>] abstract Create: op: 'TOp * fn: 'TFn * args: 'TArgs * ?implicit: bool -> OperatorNodeCtor when 'TFn :> OperatorNodeFn

    type ParenthesisNode =
        ParenthesisNode<MathNode>

    type [<AllowNullLiteral>] ParenthesisNode<'TContent when 'TContent :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isParenthesisNode: bool with get, set
        abstract content: 'TContent with get, set

    type [<AllowNullLiteral>] ParenthesisNodeCtor =
        interface end

    type [<AllowNullLiteral>] ParenthesisNodeCtorStatic =
        [<EmitConstructor>] abstract Create: content: 'TContent -> ParenthesisNodeCtor when 'TContent :> MathNode

    type RangeNode =
        RangeNode<MathNode, MathNode, MathNode>

    type RangeNode<'TStart when 'TStart :> MathNode> =
        RangeNode<'TStart, MathNode, MathNode>

    type RangeNode<'TStart, 'TEnd when 'TStart :> MathNode and 'TEnd :> MathNode> =
        RangeNode<'TStart, 'TEnd, MathNode>

    type [<AllowNullLiteral>] RangeNode<'TStart, 'TEnd, 'TStep when 'TStart :> MathNode and 'TEnd :> MathNode and 'TStep :> MathNode> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isRangeNode: bool with get, set
        abstract start: 'TStart with get, set
        abstract ``end``: 'TEnd with get, set
        abstract step: 'TStep option with get, set

    type [<AllowNullLiteral>] RangeNodeCtor =
        interface end

    type [<AllowNullLiteral>] RangeNodeCtorStatic =
        [<EmitConstructor>] abstract Create: start: 'TStart * ``end``: 'TEnd * ?step: 'TStep -> RangeNodeCtor when 'TStart :> MathNode and 'TEnd :> MathNode and 'TStep :> MathNode

    type RelationalNode =
        RelationalNode<ResizeArray<MathNode>>

    type [<AllowNullLiteral>] RelationalNode<'TParams> =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isRelationalNode: bool with get, set
        abstract conditionals: ResizeArray<string> with get, set
        abstract ``params``: 'TParams with get, set

    type [<AllowNullLiteral>] RelationalNodeCtor =
        interface end

    type [<AllowNullLiteral>] RelationalNodeCtorStatic =
        [<EmitConstructor>] abstract Create: conditionals: ResizeArray<string> * ``params``: 'TParams -> RelationalNodeCtor

    type [<AllowNullLiteral>] SymbolNode =
        inherit MathNode
        abstract ``type``: string with get, set
        abstract isSymbolNode: bool with get, set
        abstract name: string with get, set

    type [<AllowNullLiteral>] SymbolNodeCtor =
        abstract onUndefinedSymbol: (string -> obj option) with get, set

    type [<AllowNullLiteral>] SymbolNodeCtorStatic =
        [<EmitConstructor>] abstract Create: name: string -> SymbolNodeCtor

    [<Obsolete("since version 11.3. Prefer `MathNode` instead")>]
    type MathNodeCommon =
        MathNode

    type MathJsFunctionName =
        KeyOf<MathJsStatic>

    type [<AllowNullLiteral>] LUDecomposition =
        abstract L: MathCollection with get, set
        abstract U: MathCollection with get, set
        abstract p: ResizeArray<float> with get, set

    type [<AllowNullLiteral>] SLUDecomposition =
        inherit LUDecomposition
        abstract q: ResizeArray<float> with get, set

    type [<AllowNullLiteral>] QRDecomposition =
        abstract Q: MathCollection with get, set
        abstract R: MathCollection with get, set

    type [<AllowNullLiteral>] SchurDecomposition =
        abstract U: MathCollection with get, set
        abstract T: MathCollection with get, set

    type [<AllowNullLiteral>] FractionDefinition =
        abstract a: float with get, set
        abstract b: float with get, set

    type [<AllowNullLiteral>] MathJsStatic =
        inherit FactoryDependencies
        abstract e: float with get, set
        abstract pi: float with get, set
        abstract i: float with get, set
        abstract Infinity: float with get, set
        abstract LN2: float with get, set
        abstract LN10: float with get, set
        abstract LOG2E: float with get, set
        abstract LOG10E: float with get, set
        abstract NaN: float with get, set
        abstract phi: float with get, set
        abstract SQRT1_2: float with get, set
        abstract SQRT2: float with get, set
        abstract tau: float with get, set
        abstract Node: NodeCtor with get, set
        abstract AccessorNode: AccessorNodeCtor with get, set
        abstract ArrayNode: ArrayNodeCtor with get, set
        abstract AssignmentNode: AssignmentNodeCtor with get, set
        abstract BlockNode: BlockNodeCtor with get, set
        abstract ConditionalNode: ConditionalNodeCtor with get, set
        abstract ConstantNode: ConstantNodeCtor with get, set
        abstract FunctionAssignmentNode: FunctionAssignmentNodeCtor with get, set
        abstract FunctionNode: FunctionNodeCtor with get, set
        abstract IndexNode: IndexNodeCtor with get, set
        abstract ObjectNode: ObjectNodeCtor with get, set
        abstract OperatorNode: OperatorNodeCtor with get, set
        abstract ParenthesisNode: ParenthesisNodeCtor with get, set
        abstract RangeNode: RangeNodeCtor with get, set
        abstract RelationalNode: RelationalNodeCtor with get, set
        abstract SymbolNode: SymbolNodeCtor with get, set
        abstract Matrix: MatrixCtor with get, set
        /// <summary>
        /// If null were to be included in this interface, it would be
        /// auto-suggested as an import in VSCode. This causes issues because
        /// <c>null</c> is not a valid label.
        /// </summary>
        /// <seealso href="https://github.com/josdejong/mathjs/issues/2019" />
        abstract uninitialized: obj option with get, set
        abstract version: string with get, set
        abstract expression: MathNode with get, set
        /// Returns reviver function that can be used as reviver in JSON.parse function.
        abstract reviver: unit -> (obj option -> obj option -> obj option)
        /// Returns replacer function that can be used as replacer in JSON.stringify function.
        abstract replacer: unit -> (obj option -> obj option -> obj option)
        /// <summary>
        /// Set configuration options for math.js, and get current options. Will
        /// emit a ‘config’ event, with arguments (curr, prev, changes).
        /// </summary>
        /// <param name="options">
        /// Available options: {number} epsilon Minimum relative
        /// difference between two compared values, used by all comparison
        /// functions. {string} matrix A string ‘Matrix’ (default) or ‘Array’.
        /// {string} number A string ‘number’ (default), ‘BigNumber’, or
        /// ‘Fraction’ {number} precision The number of significant digits for
        /// BigNumbers. Not applicable for Numbers. {string} parenthesis How to
        /// display parentheses in LaTeX and string output. {string} randomSeed
        /// Random seed for seeded pseudo random number generator. Set to null to
        /// randomly seed.
        /// </param>
        /// <returns>Returns the current configuration</returns>
        abstract config: (ConfigOptions -> ConfigOptions) with get, set
        /// <summary>
        /// Create a typed-function which checks the types of the arguments and
        /// can match them against multiple provided signatures. The
        /// typed-function automatically converts inputs in order to find a
        /// matching signature. Typed functions throw informative errors in case
        /// of wrong input arguments.
        /// </summary>
        /// <param name="name">Optional name for the typed-function</param>
        /// <param name="signatures">Object with one or multiple function signatures</param>
        /// <returns>The created typed-function.</returns>
        abstract typed: (string -> Record<string, (ResizeArray<obj option> -> obj option)> -> (ResizeArray<obj option> -> obj option)) with get, set
        /// <summary>
        /// Create a BigNumber, which can store numbers with arbitrary precision.
        /// When a matrix is provided, all elements will be converted to
        /// BigNumber.
        /// </summary>
        /// <param name="x">Value for the big number, 0 by default.</param>
        /// <returns>The created bignumber</returns>
        abstract bignumber: ?x: U5<float, string, Fraction, BigNumber, bool> -> BigNumber
        abstract bignumber: x: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Create a boolean or convert a string or number to a boolean. In case
        /// of a number, true is returned for non-zero numbers, and false in case
        /// of zero. Strings can be 'true' or 'false', or can contain a number.
        /// When value is a matrix, all elements will be converted to boolean.
        /// </summary>
        /// <param name="x">A value of any type</param>
        /// <returns>The boolean value</returns>
        abstract boolean: x: U3<string, float, bool> option -> bool
        abstract boolean: x: MathCollection -> MathCollection
        /// <summary>
        /// Wrap any value in a chain, allowing to perform chained operations on
        /// the value. All methods available in the math.js library can be called
        /// upon the chain, and then will be evaluated with the value itself as
        /// first argument. The chain can be closed by executing chain.done(),
        /// which returns the final value. The chain has a number of special
        /// functions: done() Finalize the chain and return the chain's value.
        /// valueOf() The same as done() toString() Executes math.format() onto
        /// the chain's value, returning a string representation of the value.
        /// </summary>
        /// <param name="value">
        /// A value of any type on which to start a chained
        /// operation.
        /// </param>
        /// <returns>The created chain</returns>
        abstract chain: ?value: 'TValue -> MathJsChain<'TValue>
        /// <summary>Create a complex value or convert a value to a complex value.</summary>
        /// <param name="args">
        /// Arguments specifying the real and imaginary part of the
        /// complex number
        /// </param>
        /// <returns>Returns a complex value</returns>
        abstract complex: ?arg: U3<MathNumericType, string, PolarCoordinates> -> Complex
        abstract complex: ?arg: MathCollection -> MathCollection
        /// <param name="re">Argument specifying the real part of the complex number</param>
        /// <param name="im">
        /// Argument specifying the imaginary part of the complex
        /// number
        /// </param>
        /// <returns>Returns a complex value</returns>
        abstract complex: re: float * im: float -> Complex
        /// <summary>Create a user-defined unit and register it with the Unit type.</summary>
        /// <param name="name">The name of the new unit. Must be unique. Example: ‘knot’</param>
        /// <param name="definition">
        /// Definition of the unit in terms of existing units.
        /// For example, ‘0.514444444 m / s’.
        /// </param>
        /// <param name="options">
        /// (optional) An object containing any of the following
        /// properties:&lt;/br&gt;- prefixes {string} “none”, “short”, “long”,
        /// “binary_short”, or “binary_long”. The default is “none”.&lt;/br&gt;-
        /// aliases {Array} Array of strings. Example: [‘knots’, ‘kt’,
        /// ‘kts’]&lt;/br&gt;- offset {Numeric} An offset to apply when converting from
        /// the unit. For example, the offset for celsius is 273.15. Default is
        /// 0.
        /// </param>
        /// <returns>The new unit</returns>
        abstract createUnit: name: string * ?definition: U3<string, UnitDefinition, Unit> * ?options: CreateUnitOptions -> Unit
        /// <summary>Create a user-defined unit and register it with the Unit type.</summary>
        /// <param name="units">Definition of the unit</param>
        /// <param name="options" />
        /// <returns>The new unit</returns>
        abstract createUnit: units: Record<string, U3<string, UnitDefinition, Unit>> * ?options: CreateUnitOptions -> Unit
        /// <summary>Create a fraction convert a value to a fraction.</summary>
        /// <param name="args">
        /// Arguments specifying the numerator and denominator of the
        /// fraction
        /// </param>
        /// <returns>Returns a fraction</returns>
        abstract fraction: value: U5<float, string, BigNumber, Fraction, FractionDefinition> -> Fraction
        abstract fraction: values: MathCollection -> MathCollection
        /// <param name="numerator">Argument specifying the numerator of the fraction</param>
        /// <param name="denominator">
        /// Argument specifying the denominator of the
        /// fraction
        /// </param>
        /// <returns>Returns a fraction</returns>
        abstract fraction: numerator: float * denominator: float -> Fraction
        /// <summary>
        /// Create an index. An Index can store ranges having start, step, and
        /// end for multiple dimensions. Matrix.get, Matrix.set, and math.subset
        /// accept an Index as input.
        /// </summary>
        /// <param name="ranges">Zero or more ranges or numbers.</param>
        /// <returns>Returns the created index</returns>
        abstract index: [<ParamArray>] ranges: obj option[] -> Index
        /// <summary>
        /// Create a Matrix. The function creates a new math.type.Matrix object
        /// from an Array. A Matrix has utility functions to manipulate the data
        /// in the matrix, like getting the size and getting or setting values in
        /// the matrix. Supported storage formats are 'dense' and 'sparse'.
        /// </summary>
        /// <param name="format">The Matrix storage format</param>
        /// <returns>The created Matrix</returns>
        abstract matrix: ?format: MathJsStaticMatrix -> Matrix
        /// <param name="data">A multi dimensional array</param>
        /// <param name="format">The Matrix storage format</param>
        /// <param name="dataType">The Matrix data type</param>
        /// <returns>The created Matrix</returns>
        abstract matrix: data: U2<MathCollection, ResizeArray<string>> * ?format: MathJsStaticMatrix * ?dataType: string -> Matrix
        /// <summary>
        /// Create a number or convert a string, boolean, or unit to a number.
        /// When value is a matrix, all elements will be converted to number.
        /// </summary>
        /// <param name="value">Value to be converted</param>
        /// <returns>The created number</returns>
        abstract number: ?value: U6<string, float, BigNumber, Fraction, bool, Unit> -> float
        abstract number: ?value: MathCollection -> U2<float, MathCollection>
        /// <param name="value">Value to be converted</param>
        /// <param name="valuelessUnit">
        /// A valueless unit, used to convert a unit to a
        /// number
        /// </param>
        /// <returns>The created number</returns>
        abstract number: unit: Unit * valuelessUnit: U2<Unit, string> -> float
        /// <summary>
        /// Create a Sparse Matrix. The function creates a new math.type.Matrix
        /// object from an Array. A Matrix has utility functions to manipulate
        /// the data in the matrix, like getting the size and getting or setting
        /// values in the matrix.
        /// </summary>
        /// <param name="data">A two dimensional array</param>
        /// <param name="dataType">Sparse Matrix data type</param>
        /// <returns>The created matrix</returns>
        abstract sparse: ?data: MathCollection * ?dataType: string -> Matrix
        /// <summary>
        /// Split a unit in an array of units whose sum is equal to the original
        /// unit.
        /// </summary>
        /// <param name="unit">A unit to be split</param>
        /// <param name="parts">An array of strings or valueless units</param>
        /// <returns>An array of units</returns>
        abstract splitUnit: unit: Unit * parts: ResizeArray<Unit> -> ResizeArray<Unit>
        /// <summary>
        /// Create a string or convert any object into a string. Elements of
        /// Arrays and Matrices are processed element wise.
        /// </summary>
        /// <param name="value">A value to convert to a string</param>
        /// <returns>The created string</returns>
        abstract string: value: U3<MathNumericType, string, Unit> option -> string
        abstract string: value: MathCollection -> MathCollection
        /// <summary>
        /// Create a unit. Depending on the passed arguments, the function will
        /// create and return a new math.type.Unit object. When a matrix is
        /// provided, all elements will be converted to units.
        /// </summary>
        /// <param name="unit">The unit to be created</param>
        /// <returns>The created unit</returns>
        abstract unit: unit: string -> Unit
        /// <param name="unit">The unit to be created</param>
        /// <returns>The created unit</returns>
        abstract unit: unit: Unit -> Unit
        /// <param name="value">The value of the unit to be created</param>
        /// <param name="unit">The unit to be created</param>
        /// <returns>The created unit</returns>
        abstract unit: value: MathNumericType * unit: string -> Unit
        abstract unit: value: MathCollection * unit: string -> ResizeArray<Unit>
        /// <summary>
        /// Parse and compile an expression. Returns a an object with a function
        /// evaluate([scope]) to evaluate the compiled expression.
        /// </summary>
        /// <param name="expr">The expression to be compiled</param>
        /// <returns>An object with the compiled expression</returns>
        abstract compile: expr: MathExpression -> EvalFunction
        /// <param name="exprs">The expressions to be compiled</param>
        /// <returns>An array of objects with the compiled expressions</returns>
        abstract compile: exprs: ResizeArray<MathExpression> -> ResizeArray<EvalFunction>
        /// <summary>Evaluate an expression.</summary>
        /// <param name="expr">The expression to be evaluated</param>
        /// <param name="scope">Scope to read/write variables</param>
        /// <returns>The result of the expression</returns>
        abstract evaluate: expr: U2<MathExpression, Matrix> * ?scope: obj -> obj option
        abstract evaluate: expr: ResizeArray<MathExpression> * ?scope: obj -> ResizeArray<obj option>
        /// <summary>
        /// Retrieve help on a function or data type. Help files are retrieved
        /// from the documentation in math.expression.docs.
        /// </summary>
        /// <param name="search">A function or function name for which to get help</param>
        /// <returns>A help object</returns>
        abstract help: search: (unit -> obj option) -> Help
        /// Parse an expression. Returns a node tree, which can be evaluated by
        /// invoking node.evaluate();
        abstract parse: ParseFunction with get, set
        /// <summary>
        /// Create a parser. The function creates a new math.expression.Parser
        /// object.
        /// </summary>
        /// <returns>A Parser object</returns>
        abstract parser: unit -> Parser
        /// <param name="expr">The expression to differentiate</param>
        /// <param name="variable">The variable over which to differentiate</param>
        /// <param name="options">
        /// There is one option available, simplify, which is true
        /// by default. When false, output will not be simplified.
        /// </param>
        /// <returns>The derivative of expr</returns>
        abstract derivative: expr: U2<MathNode, string> * variable: U2<MathNode, string> * ?options: {| simplify: bool |} -> MathNode
        /// <summary>
        /// Solves the linear equation system by forwards substitution. Matrix
        /// must be a lower triangular matrix.
        /// </summary>
        /// <param name="L">A N x N matrix or array (L)</param>
        /// <param name="b">A column vector with the b values</param>
        /// <returns>A column vector with the linear system solution (x)</returns>
        abstract lsolve: L: Matrix * b: MathCollection -> Matrix
        abstract lsolve: L: MathArray * b: MathCollection -> MathArray
        /// <summary>
        /// Calculate the Matrix LU decomposition with partial pivoting. Matrix A
        /// is decomposed in two matrices (L, U) and a row permutation vector p
        /// where A[p,:] = L * U
        /// </summary>
        /// <param name="A">
        /// A two dimensional matrix or array for which to get the LUP
        /// decomposition.
        /// </param>
        /// <returns>
        /// The lower triangular matrix, the upper triangular matrix and
        /// the permutation matrix.
        /// </returns>
        abstract lup: ?A: MathCollection -> LUDecomposition
        /// <summary>
        /// Solves the linear system A * x = b where A is an [n x n] matrix and b
        /// is a [n] column vector.
        /// </summary>
        /// <param name="A">Invertible Matrix or the Matrix LU decomposition</param>
        /// <param name="b">Column Vector</param>
        /// <param name="order">
        /// The Symbolic Ordering and Analysis order, see slu for
        /// details. Matrix must be a SparseMatrix
        /// </param>
        /// <param name="threshold">
        /// Partial pivoting threshold (1 for partial pivoting),
        /// see slu for details. Matrix must be a SparseMatrix.
        /// </param>
        /// <returns>
        /// Column vector with the solution to the linear system A * x =
        /// b
        /// </returns>
        abstract lusolve: A: Matrix * b: MathCollection * ?order: float * ?threshold: float -> Matrix
        abstract lusolve: A: MathArray * b: MathCollection * ?order: float * ?threshold: float -> MathArray
        abstract lusolve: A: LUDecomposition * b: MathCollection -> Matrix
        abstract polynomialRoot: constantCoeff: U2<float, Complex> * linearCoeff: U2<float, Complex> * ?quadraticCoeff: U2<float, Complex> * ?cubicCoeff: U2<float, Complex> -> ResizeArray<U2<float, Complex>>
        /// <summary>
        /// Calculate the Matrix QR decomposition. Matrix A is decomposed in two
        /// matrices (Q, R) where Q is an orthogonal matrix and R is an upper
        /// triangular matrix.
        /// </summary>
        /// <param name="A">
        /// A two dimensional matrix or array for which to get the QR
        /// decomposition.
        /// </param>
        /// <returns>Q: the orthogonal matrix and R: the upper triangular matrix</returns>
        abstract qr: A: MathCollection -> QRDecomposition
        [<Emit("$0.rationalize($1,$2,false)")>] abstract rationalize_false: expr: U2<MathNode, string> * ?optional: U2<obj, bool> -> MathNode
        /// <summary>
        /// Transform a rationalizable expression in a rational fraction. If
        /// rational fraction is one variable polynomial then converts the
        /// numerator and denominator in canonical form, with decreasing
        /// exponents, returning the coefficients of numerator.
        /// </summary>
        /// <param name="expr">The expression to check if is a polynomial expression</param>
        /// <param name="optional">
        /// scope of expression or true for already evaluated
        /// rational expression at input
        /// </param>
        /// <param name="detailed">
        /// optional True if return an object, false if return
        /// expression node (default)
        /// </param>
        /// <returns>The rational polynomial of expr</returns>
        [<Emit("$0.rationalize($1,$2,true)")>] abstract rationalize_true: expr: U2<MathNode, string> * ?optional: U2<obj, bool> -> {| expression: U2<MathNode, string>; variables: ResizeArray<string>; coefficients: ResizeArray<MathType> |}
        /// <summary>Simplify an expression tree.</summary>
        /// <param name="expr">The expression to be simplified</param>
        /// <param name="rules">
        /// (optional) A list of rules are applied to an expression, repeating
        /// over the list until no further changes are made. It’s possible to
        /// pass a custom set of rules to the function as second argument. A rule
        /// can be specified as an object, string, or function.
        /// </param>
        /// <param name="scope">(optional) Scope to variables</param>
        /// <param name="options">(optional) An object with simplify options</param>
        /// <returns>Returns the simplified form of expr</returns>
        abstract simplify: Simplify with get, set
        abstract simplifyConstant: expr: U2<MathNode, string> * ?options: SimplifyOptions -> MathNode
        abstract simplifyCore: expr: U2<MathNode, string> * ?options: SimplifyOptions -> MathNode
        /// <summary>Replaces variable nodes with their scoped values</summary>
        /// <param name="node">Tree to replace variable nodes in</param>
        /// <param name="scope">Scope to read/write variables</param>
        abstract resolve: node: U2<MathNode, string> * ?scope: Record<string, obj option> -> MathNode
        abstract resolve: node: ResizeArray<U2<MathNode, string>> * ?scope: Record<string, obj option> -> ResizeArray<MathNode>
        abstract resolve: node: Matrix * ?scope: Record<string, obj option> -> Matrix
        /// <summary>
        /// Calculate the Sparse Matrix LU decomposition with full pivoting.
        /// Sparse Matrix A is decomposed in two matrices (L, U) and two
        /// permutation vectors (pinv, q) where P * A * Q = L * U
        /// </summary>
        /// <param name="A">
        /// A two dimensional sparse matrix for which to get the LU
        /// decomposition.
        /// </param>
        /// <param name="order">
        /// The Symbolic Ordering and Analysis order: 0 - Natural
        /// ordering, no permutation vector q is returned 1 - Matrix must be
        /// square, symbolic ordering and analisis is performed on M = A + A' 2 -
        /// Symbolic ordering and analysis is performed on M = A' * A. Dense
        /// columns from A' are dropped, A recreated from A'. This is appropriate
        /// for LU factorization of non-symmetric matrices. 3 - Symbolic ordering
        /// and analysis is performed on M = A' * A. This is best used for LU
        /// factorization is matrix M has no dense rows. A dense row is a row
        /// with more than 10*sqr(columns) entries.
        /// </param>
        /// <param name="threshold">Partial pivoting threshold (1 for partial pivoting)</param>
        /// <returns>
        /// The lower triangular matrix, the upper triangular matrix and
        /// the permutation vectors.
        /// </returns>
        abstract slu: A: Matrix * order: float * threshold: float -> SLUDecomposition
        /// <summary>
        /// Solves the linear equation system by backward substitution. Matrix
        /// must be an upper triangular matrix. U * x = b
        /// </summary>
        /// <param name="U">A N x N matrix or array (U)</param>
        /// <param name="b">A column vector with the b values</param>
        /// <returns>A column vector with the linear system solution (x)</returns>
        abstract usolve: U: Matrix * b: MathCollection -> Matrix
        abstract usolve: U: MathArray * b: MathCollection -> MathArray
        /// <summary>
        /// Calculate the absolute value of a number. For matrices, the function
        /// is evaluated element wise.
        /// </summary>
        /// <param name="x">A number or matrix for which to get the absolute value</param>
        /// <returns>Absolute value of x</returns>
        abstract abs: x: 'T -> 'T when 'T :> MathType
        /// <summary>
        /// Add two values, x + y. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">First value to add</param>
        /// <param name="y">Second value to add</param>
        /// <returns>Sum of x and y</returns>
        abstract add: x: 'T * y: 'T -> 'T when 'T :> MathType
        abstract add: x: MathType * y: MathType -> MathType
        /// <summary>Calculate the cubic root of a value.</summary>
        /// <param name="x">Value for which to calculate the cubic root.</param>
        /// <param name="allRoots">
        /// Optional, false by default. Only applicable when x is
        /// a number or complex number. If true, all complex roots are returned,
        /// if false (default) the principal root is returned.
        /// </param>
        /// <returns>Returns the cubic root of x</returns>
        abstract cbrt: x: Complex * ?allRoots: bool -> Complex
        abstract cbrt: x: 'T -> 'T
        /// <summary>
        /// Round a value towards plus infinity If x is complex, both real and
        /// imaginary part are rounded towards plus infinity. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="x">Number to be rounded</param>
        /// <param name="n">Number of decimals Default value: 0.</param>
        /// <returns>Rounded value</returns>
        abstract ceil: x: 'T * ?n: U2<float, BigNumber> -> NoLiteralType<'T>
        abstract ceil: x: MathNumericType * n: 'U -> 'U when 'U :> MathCollection
        /// <summary>
        /// Round a value towards zero. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">Number to be rounded</param>
        /// <param name="n">Number of decimals Default value: 0.</param>
        /// <returns>Rounded value</returns>
        abstract fix: x: 'T * ?n: U2<float, BigNumber> -> NoLiteralType<'T>
        abstract fix: x: MathNumericType * n: 'U -> 'U when 'U :> MathCollection
        /// <summary>
        /// Round a value towards minus infinity. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">Number to be rounded</param>
        /// <param name="n">Number of decimals Default value: 0.</param>
        /// <returns>Rounded value</returns>
        abstract floor: x: 'T * ?n: U2<float, BigNumber> -> NoLiteralType<'T>
        abstract floor: x: MathNumericType * n: 'U -> 'U when 'U :> MathCollection
        /// <summary>
        /// Round a value towards the nearest integer. For matrices, the function
        /// is evaluated element wise.
        /// </summary>
        /// <param name="x">Number to be rounded</param>
        /// <param name="n">Number of decimals Default value: 0.</param>
        /// <returns>Rounded value of x</returns>
        abstract round: x: 'T * ?n: U2<float, BigNumber> -> NoLiteralType<'T>
        abstract round: x: MathNumericType * n: 'U -> 'U when 'U :> MathCollection
        /// <summary>
        /// Compute the cube of a value, x * x * x. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">Number for which to calculate the cube</param>
        /// <returns>Cube of x</returns>
        abstract cube: x: 'T -> 'T
        /// <summary>
        /// Divide two values, x / y. To divide matrices, x is multiplied with
        /// the inverse of y: x * inv(y).
        /// </summary>
        /// <param name="x">Numerator</param>
        /// <param name="y">Denominator</param>
        /// <returns>Quotient, x / y</returns>
        abstract divide: x: Unit * y: Unit -> U2<Unit, float>
        abstract divide: x: Unit * y: float -> Unit
        abstract divide: x: float * y: float -> float
        abstract divide: x: MathType * y: MathType -> MathType
        /// <summary>
        /// Divide two matrices element wise. The function accepts both matrices
        /// and scalar values.
        /// </summary>
        /// <param name="x">Numerator</param>
        /// <param name="y">Denominator</param>
        /// <returns>Quotient, x ./ y</returns>
        abstract dotDivide: x: 'T * y: MathType -> 'T when 'T :> MathCollection
        abstract dotDivide: x: MathType * y: 'T -> 'T when 'T :> MathCollection
        abstract dotDivide: x: Unit * y: MathType -> Unit
        abstract dotDivide: x: MathType * y: Unit -> Unit
        abstract dotDivide: x: MathNumericType * y: MathNumericType -> MathNumericType
        /// <summary>
        /// Multiply two matrices element wise. The function accepts both
        /// matrices and scalar values.
        /// </summary>
        /// <param name="x">Left hand value</param>
        /// <param name="y">Right hand value</param>
        /// <returns>Multiplication of x and y</returns>
        abstract dotMultiply: x: 'T * y: MathType -> 'T when 'T :> MathCollection
        abstract dotMultiply: x: MathType * y: 'T -> 'T when 'T :> MathCollection
        abstract dotMultiply: x: Unit * y: MathType -> Unit
        abstract dotMultiply: x: MathType * y: Unit -> Unit
        abstract dotMultiply: x: MathNumericType * y: MathNumericType -> MathNumericType
        /// <summary>Calculates the power of x to y element wise.</summary>
        /// <param name="x">The base</param>
        /// <param name="y">The exponent</param>
        /// <returns>The value of x to the power y</returns>
        abstract dotPow: x: 'T * y: MathType -> 'T when 'T :> MathType
        /// <summary>
        /// Calculate the exponent of a value. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">A number or matrix to exponentiate</param>
        /// <returns>Exponent of x</returns>
        abstract exp: x: 'T -> 'T
        /// <summary>
        /// Calculate the value of subtracting 1 from the exponential value. For
        /// matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">A number or matrix to apply expm1</param>
        /// <returns>Exponent of x</returns>
        abstract expm1: x: 'T -> 'T
        /// <summary>
        /// Calculate the greatest common divisor for two or more values or
        /// arrays. For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="args">Two or more integer numbers</param>
        /// <returns>The greatest common divisor</returns>
        abstract gcd: [<ParamArray>] args: 'T[] -> 'T
        abstract gcd: args: ResizeArray<'T> -> 'T
        /// <summary>
        /// Calculate the hypotenusa of a list with values. The hypotenusa is
        /// defined as: hypot(a, b, c, ...) = sqrt(a^2 + b^2 + c^2 + ...) For
        /// matrix input, the hypotenusa is calculated for all values in the
        /// matrix.
        /// </summary>
        /// <param name="args">
        /// A list with numeric values or an Array or Matrix. Matrix
        /// and Array input is flattened and returns a single number for the
        /// whole matrix.
        /// </param>
        /// <returns>Returns the hypothenuse of the input values.</returns>
        abstract hypot: [<ParamArray>] args: 'T[] -> 'T
        /// <summary>
        /// Calculate the least common multiple for two or more values or arrays.
        /// lcm is defined as: lcm(a, b) = abs(a * b) / gcd(a, b) For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="a">An integer number</param>
        /// <param name="b">An integer number</param>
        /// <returns>The least common multiple</returns>
        abstract lcm: a: 'T * b: 'T -> 'T
        /// <summary>Calculate the logarithm of a value.</summary>
        /// <param name="x">Value for which to calculate the logarithm.</param>
        /// <param name="base">
        /// Optional base for the logarithm. If not provided, the
        /// natural logarithm of x is calculated. Default value: e.
        /// </param>
        /// <returns>Returns the logarithm of x</returns>
        abstract log: x: 'T * ?``base``: U3<float, BigNumber, Complex> -> NoLiteralType<'T>
        /// <summary>
        /// Calculate the 10-base of a value. This is the same as calculating
        /// log(x, 10). For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">Value for which to calculate the logarithm.</param>
        /// <returns>Returns the 10-base logarithm of x</returns>
        abstract log10: x: 'T -> 'T
        /// <summary>
        /// Calculate the logarithm of a value+1. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">Value for which to calculate the logarithm.</param>
        /// <returns>Returns the logarithm of x+1</returns>
        abstract log1p: x: 'T * ?``base``: U3<float, BigNumber, Complex> -> 'T
        /// <summary>
        /// Calculate the 2-base of a value. This is the same as calculating
        /// log(x, 2). For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">Value for which to calculate the logarithm.</param>
        /// <returns>Returns the 2-base logarithm of x</returns>
        abstract log2: x: 'T -> 'T
        /// <summary>
        /// Calculates the modulus, the remainder of an integer division. For
        /// matrices, the function is evaluated element wise. The modulus is
        /// defined as: x - y * floor(x / y)
        /// </summary>
        /// <seealso href="http://en.wikipedia.org/wiki/Modulo_operation." />
        /// <param name="x">Dividend</param>
        /// <param name="y">Divisor</param>
        /// <returns>Returns the remainder of x divided by y</returns>
        abstract ``mod``: x: 'T * y: U4<float, BigNumber, Fraction, MathCollection> -> NoLiteralType<'T>
        /// <summary>
        /// Multiply two values, x * y. The result is squeezed. For matrices, the
        /// matrix product is calculated.
        /// </summary>
        /// <param name="x">The first value to multiply</param>
        /// <param name="y">The second value to multiply</param>
        /// <returns>Multiplication of x and y</returns>
        abstract multiply: x: 'T * y: MathType -> Matrix when 'T :> Matrix
        abstract multiply: x: MathType * y: 'T -> Matrix when 'T :> Matrix
        abstract multiply: x: 'T * y: ResizeArray<'T> -> 'T
        abstract multiply: x: ResizeArray<'T> * y: 'T -> 'T
        abstract multiply: x: 'T * y: 'T -> 'T when 'T :> MathArray
        abstract multiply: x: Unit * y: Unit -> Unit
        abstract multiply: x: float * y: float -> float
        abstract multiply: x: MathType * y: MathType -> MathType
        /// <summary>
        /// Calculate the norm of a number, vector or matrix. The second
        /// parameter p is optional. If not provided, it defaults to 2.
        /// </summary>
        /// <param name="x">Value for which to calculate the norm</param>
        /// <param name="p">
        /// Vector space. Supported numbers include Infinity and
        /// -Infinity. Supported strings are: 'inf', '-inf', and 'fro' (The
        /// Frobenius norm) Default value: 2.
        /// </param>
        /// <returns>the p-norm</returns>
        abstract norm: x: U4<float, BigNumber, Complex, MathCollection> * ?p: U3<float, BigNumber, string> -> U2<float, BigNumber>
        /// <summary>
        /// Calculate the nth root of a value. The principal nth root of a
        /// positive real number A, is the positive real solution of the equation
        /// x^root = A For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="a">Value for which to calculate the nth root</param>
        /// <param name="root">The root. Default value: 2.</param>
        /// <returns>The nth root of a</returns>
        abstract nthRoot: a: U4<float, BigNumber, MathCollection, Complex> * ?root: U2<float, BigNumber> -> U3<float, Complex, MathCollection>
        /// <summary>
        /// Calculates the power of x to y, x ^ y. Matrix exponentiation is
        /// supported for square matrices x, and positive integer exponents y.
        /// </summary>
        /// <param name="x">The base</param>
        /// <param name="y">The exponent</param>
        /// <returns>x to the power y</returns>
        abstract pow: x: MathType * y: U3<float, BigNumber, Complex> -> MathType
        /// <summary>
        /// Compute the sign of a value. The sign of a value x is: 1 when x &gt; 1
        /// -1 when x &lt; 0 0 when x == 0 For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">The number for which to determine the sign</param>
        /// <returns>The sign of x</returns>
        abstract sign: x: 'T -> 'T when 'T :> MathType
        /// <summary>
        /// Calculate the square root of a value. For matrices, use either
        /// sqrtm for the matrix square root, or map(M, sqrt) to take the
        /// square root element wise.
        /// </summary>
        /// <param name="x">Value for which to calculate the square root</param>
        /// <returns>Returns the square root of x</returns>
        abstract sqrt: x: float -> U2<float, Complex>
        abstract sqrt: x: 'T -> 'T
        /// <summary>Compute the square of a value, x * x.</summary>
        /// <param name="x">Number for which to calculate the square</param>
        /// <returns>Squared value</returns>
        abstract square: x: 'T -> 'T
        /// <summary>
        /// Subtract two values, x - y. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">Initial value</param>
        /// <param name="y">Value to subtract from x</param>
        /// <returns>Subtraction of x and y</returns>
        abstract subtract: x: 'T * y: 'T -> 'T when 'T :> MathType
        abstract subtract: x: MathType * y: MathType -> MathType
        /// <summary>
        /// Inverse the sign of a value, apply a unary minus operation. For
        /// matrices, the function is evaluated element wise. Boolean values and
        /// strings will be converted to a number. For complex numbers, both real
        /// and complex value are inverted.
        /// </summary>
        /// <param name="x">Number to be inverted</param>
        /// <returns>Retursn the value with inverted sign</returns>
        abstract unaryMinus: x: 'T -> 'T when 'T :> MathType
        /// <summary>
        /// Unary plus operation. Boolean values and strings will be converted to
        /// a number, numeric values will be returned as is. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="x">Input value</param>
        /// <returns>
        /// Returns the input value when numeric, converts to a number
        /// when input is non-numeric.
        /// </returns>
        abstract unaryPlus: x: 'T -> 'T
        /// <summary>
        /// Calculate the extended greatest common divisor for two values. See
        /// <see href="http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm." />
        /// </summary>
        /// <param name="a">An integer number</param>
        /// <param name="b">An integer number</param>
        /// <returns>
        /// Returns an array containing 3 integers [div, m, n] where div
        /// = gcd(a, b) and a*m + b*n = div
        /// </returns>
        abstract xgcd: a: U2<float, BigNumber> * b: U2<float, BigNumber> -> MathArray
        /// <summary>
        /// Bitwise AND two values, x &amp; y. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">First value to and</param>
        /// <param name="y">Second value to and</param>
        /// <returns>AND of x and y</returns>
        abstract bitAnd: x: 'T * y: U3<float, BigNumber, MathCollection> -> NoLiteralType<'T>
        /// <summary>
        /// Bitwise NOT value, ~x. For matrices, the function is evaluated
        /// element wise. For units, the function is evaluated on the best prefix
        /// base.
        /// </summary>
        /// <param name="x">Value to not</param>
        /// <returns>NOT of x</returns>
        abstract bitNot: x: 'T -> 'T
        /// <summary>
        /// Bitwise OR two values, x | y. For matrices, the function is evaluated
        /// element wise. For units, the function is evaluated on the lowest
        /// print base.
        /// </summary>
        /// <param name="x">First value to or</param>
        /// <param name="y">Second value to or</param>
        /// <returns>OR of x and y</returns>
        abstract bitOr: x: 'T * y: 'T -> 'T
        /// <summary>
        /// Bitwise XOR two values, x ^ y. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">First value to xor</param>
        /// <param name="y">Second value to xor</param>
        /// <returns>XOR of x and y</returns>
        abstract bitXor: x: 'T * y: U3<float, BigNumber, MathCollection> -> NoLiteralType<'T>
        /// <summary>
        /// Bitwise left logical shift of a value x by y number of bits, x &lt;&lt; y.
        /// For matrices, the function is evaluated element wise. For units, the
        /// function is evaluated on the best prefix base.
        /// </summary>
        /// <param name="x">Value to be shifted</param>
        /// <param name="y">Amount of shifts</param>
        /// <returns>x shifted left y times</returns>
        abstract leftShift: x: 'T * y: U2<float, BigNumber> -> NoLiteralType<'T>
        /// <summary>
        /// Bitwise right arithmetic shift of a value x by y number of bits, x &gt;&gt;
        /// y. For matrices, the function is evaluated element wise. For units,
        /// the function is evaluated on the best prefix base.
        /// </summary>
        /// <param name="x">Value to be shifted</param>
        /// <param name="y">Amount of shifts</param>
        /// <returns>x sign-filled shifted right y times</returns>
        abstract rightArithShift: x: 'T * y: U2<float, BigNumber> -> NoLiteralType<'T>
        /// <summary>
        /// Bitwise right logical shift of value x by y number of bits, x &gt;&gt;&gt; y.
        /// For matrices, the function is evaluated element wise. For units, the
        /// function is evaluated on the best prefix base.
        /// </summary>
        /// <param name="x">Value to be shifted</param>
        /// <param name="y">Amount of shifts</param>
        /// <returns>x zero-filled shifted right y times</returns>
        abstract rightLogShift: x: 'T * y: float -> NoLiteralType<'T>
        /// <summary>
        /// The Bell Numbers count the number of partitions of a set. A partition
        /// is a pairwise disjoint subset of S whose union is S. bellNumbers only
        /// takes integer arguments. The following condition must be enforced: n
        /// &gt;= 0
        /// </summary>
        /// <param name="n">Total number of objects in the set</param>
        /// <returns>B(n)</returns>
        abstract bellNumbers: n: 'T -> 'T
        /// <summary>
        /// The Catalan Numbers enumerate combinatorial structures of many
        /// different types. catalan only takes integer arguments. The following
        /// condition must be enforced: n &gt;= 0
        /// </summary>
        /// <param name="n">nth Catalan number</param>
        /// <returns>Cn(n)</returns>
        abstract catalan: n: 'T -> 'T
        /// <summary>
        /// The composition counts of n into k parts. Composition only takes
        /// integer arguments. The following condition must be enforced: k &lt;= n.
        /// </summary>
        /// <param name="n">Total number of objects in the set</param>
        /// <param name="k">Number of objects in the subset</param>
        /// <returns>Returns the composition counts of n into k parts.</returns>
        abstract composition: n: 'T * k: U2<float, BigNumber> -> NoLiteralType<'T>
        /// <summary>
        /// The Stirling numbers of the second kind, counts the number of ways to
        /// partition a set of n labelled objects into k nonempty unlabelled
        /// subsets. stirlingS2 only takes integer arguments. The following
        /// condition must be enforced: k &lt;= n. If n = k or k = 1, then s(n,k) =
        /// 1
        /// </summary>
        /// <param name="n">Total number of objects in the set</param>
        /// <param name="k">Number of objects in the subset</param>
        /// <returns>S(n,k)</returns>
        abstract stirlingS2: n: 'T * k: U2<float, BigNumber> -> NoLiteralType<'T>
        /// <summary>
        /// Compute the argument of a complex value. For a complex number a + bi,
        /// the argument is computed as atan2(b, a). For matrices, the function
        /// is evaluated element wise.
        /// </summary>
        /// <param name="x">A complex number or array with complex numbers</param>
        /// <returns>The argument of x</returns>
        abstract arg: x: U2<float, Complex> -> float
        abstract arg: x: U2<BigNumber, Complex> -> BigNumber
        abstract arg: x: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Compute the complex conjugate of a complex value. If x = a+bi, the
        /// complex conjugate of x is a - bi. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="x">A complex number or array with complex numbers</param>
        /// <returns>The complex conjugate of x</returns>
        abstract conj: x: 'T -> NoLiteralType<'T>
        /// <summary>
        /// Get the imaginary part of a complex number. For a complex number a +
        /// bi, the function returns b. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">A complex number or array with complex numbers</param>
        /// <returns>The imaginary part of x</returns>
        abstract im: x: MathJsChain<U2<float, Complex>> -> MathJsChain<float>
        abstract im: x: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Get the real part of a complex number. For a complex number a + bi,
        /// the function returns a. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">A complex number or array of complex numbers</param>
        /// <returns>The real part of x</returns>
        abstract re: x: MathJsChain<U2<float, Complex>> -> MathJsChain<float>
        abstract re: x: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Calculates: The eucledian distance between two points in 2 and 3
        /// dimensional spaces. Distance between point and a line in 2 and 3
        /// dimensional spaces. Pairwise distance between a set of 2D or 3D
        /// points NOTE: When substituting coefficients of a line(a, b and c),
        /// use ax + by + c = 0 instead of ax + by = c For parametric equation of
        /// a 3D line, x0, y0, z0, a, b, c are from: (x−x0, y−y0, z−z0) = t(a, b,
        /// c)
        /// </summary>
        /// <param name="x">Coordinates of the first point</param>
        /// <param name="y">Coordinates of the second point</param>
        /// <returns>Returns the distance from two/three points</returns>
        abstract distance: x: U2<MathCollection, obj> * y: U2<MathCollection, obj> -> U2<float, BigNumber>
        /// <summary>
        /// Calculates the point of intersection of two lines in two or three
        /// dimensions and of a line and a plane in three dimensions. The inputs
        /// are in the form of arrays or 1 dimensional matrices. The line
        /// intersection functions return null if the lines do not meet. Note:
        /// Fill the plane coefficients as x + y + z = c and not as x + y + z + c
        /// = 0.
        /// </summary>
        /// <param name="w">Co-ordinates of first end-point of first line</param>
        /// <param name="x">Co-ordinates of second end-point of first line</param>
        /// <param name="y">
        /// Co-ordinates of first end-point of second line OR
        /// Coefficients of the plane's equation
        /// </param>
        /// <param name="z">
        /// Co-ordinates of second end-point of second line OR null if
        /// the calculation is for line and plane
        /// </param>
        /// <returns>Returns the point of intersection of lines/lines-planes</returns>
        abstract intersect: w: MathCollection * x: MathCollection * y: MathCollection * ?z: MathCollection -> MathArray
        /// <summary>
        /// Logical and. Test whether two values are both defined with a
        /// nonzero/nonempty value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">First value to and</param>
        /// <param name="y">Second value to and</param>
        /// <returns>
        /// Returns true when both inputs are defined with a
        /// nonzero/nonempty value.
        /// </returns>
        abstract ``and``: x: U5<float, BigNumber, Complex, Unit, MathCollection> * y: U5<float, BigNumber, Complex, Unit, MathCollection> -> U2<bool, MathCollection>
        /// <summary>
        /// Logical not. Flips boolean value of a given parameter. For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="x">First value to not</param>
        /// <returns>Returns true when input is a zero or empty value.</returns>
        abstract not: x: U5<float, BigNumber, Complex, Unit, MathCollection> -> U2<bool, MathCollection>
        /// <summary>
        /// Logical or. Test if at least one value is defined with a
        /// nonzero/nonempty value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">First value to or</param>
        /// <param name="y">Second value to or</param>
        /// <returns>
        /// Returns true when one of the inputs is defined with a
        /// nonzero/nonempty value.
        /// </returns>
        abstract ``or``: x: U5<float, BigNumber, Complex, Unit, MathCollection> * y: U5<float, BigNumber, Complex, Unit, MathCollection> -> U2<bool, MathCollection>
        /// <summary>
        /// Logical xor. Test whether one and only one value is defined with a
        /// nonzero/nonempty value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">First value to xor</param>
        /// <param name="y">Second value to xor</param>
        /// <returns>
        /// Returns true when one and only one input is defined with a
        /// nonzero/nonempty value.
        /// </returns>
        abstract xor: x: U5<float, BigNumber, Complex, Unit, MathCollection> * y: U5<float, BigNumber, Complex, Unit, MathCollection> -> U2<bool, MathCollection>
        /// <summary>
        /// Apply a function that maps an array to a scalar along a given axis of a
        /// matrix or array. Returns a new matrix or array with one less dimension
        /// than the input.
        /// </summary>
        /// <param name="array">The input Matrix</param>
        /// <param name="dim">The dimension along which the callback is applied</param>
        /// <param name="callback">
        /// The callback function that is applied. This Function should take an
        /// array or 1-d matrix as an input and return a number.
        /// </param>
        /// <returns>The residual matrix with the function applied over some dimension.</returns>
        abstract apply: array: 'T * dim: float * callback: (MathCollection -> float) -> 'T when 'T :> MathCollection
        /// <summary>
        /// Concatenate two or more matrices. dim: number is a zero-based
        /// dimension over which to concatenate the matrices. By default the last
        /// dimension of the matrices.
        /// </summary>
        /// <param name="args">Two or more matrices</param>
        /// <returns>Concatenated matrix</returns>
        abstract concat: [<ParamArray>] args: Array<U3<MathCollection, float, BigNumber>> -> MathCollection
        /// <summary>
        /// Calculate the cross product for two vectors in three dimensional
        /// space. The cross product of A = [a1, a2, a3] and B =[b1, b2, b3] is
        /// defined as: cross(A, B) = [ a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1
        /// * b2 - a2 * b1 ]
        /// </summary>
        /// <param name="x">First vector</param>
        /// <param name="y">Second vector</param>
        /// <returns>Returns the cross product of x and y</returns>
        abstract cross: x: MathCollection * y: MathCollection -> MathCollection
        /// <summary>Calculate the determinant of a matrix.</summary>
        /// <param name="x">A Matrix</param>
        /// <returns>the determinant of x</returns>
        abstract det: x: MathCollection -> float
        /// <summary>
        /// Create a diagonal matrix or retrieve the diagonal of a matrix. When x
        /// is a vector, a matrix with vector x on the diagonal will be returned.
        /// When x is a two dimensional matrix, the matrixes kth diagonal will be
        /// returned as vector. When k is positive, the values are placed on the
        /// super diagonal. When k is negative, the values are placed on the sub
        /// diagonal.
        /// </summary>
        /// <param name="X">A two dimensional matrix or a vector</param>
        /// <param name="k">
        /// The diagonal where the vector will be filled in or
        /// retrieved. Default value: 0.
        /// </param>
        /// <param name="format">The matrix storage format. Default value: 'dense'.</param>
        /// <returns>
        /// Diagonal matrix from input vector, or diagonal from input
        /// matrix
        /// </returns>
        abstract diag: X: MathCollection * ?format: string -> Matrix
        abstract diag: X: MathCollection * k: U2<float, BigNumber> * ?format: string -> MathCollection
        /// <summary>
        /// Calculate the dot product of two vectors. The dot product of A = [a1,
        /// a2, a3, ..., an] and B = [b1, b2, b3, ..., bn] is defined as: dot(A,
        /// B) = a1 * b1 + a2 * b2 + a3 * b3 + ... + an * bn
        /// </summary>
        /// <param name="x">First vector</param>
        /// <param name="y">Second vector</param>
        /// <returns>Returns the dot product of x and y</returns>
        abstract dot: x: MathCollection * y: MathCollection -> float
        /// <summary>
        /// Compute eigenvalues and eigenvectors of a matrix.
        /// The eigenvalues are sorted by their absolute value, ascending.
        /// An eigenvalue with multiplicity k will be listed k times.
        /// The eigenvectors are returned as columns of a matrix – the eigenvector
        /// that belongs to the j-th eigenvalue in the list (eg. values[j]) is the
        /// j-th column (eg. column(vectors, j)). If the algorithm fails to converge,
        /// it will throw an error – in that case, however, you may still find useful
        /// information in err.values and err.vectors
        /// </summary>
        /// <param name="x">Matrix to be diagonalized</param>
        /// <param name="prec">Precision, default value: 1e-15</param>
        /// <returns>Object containing an array of eigenvalues and a matrix with eigenvectors as columns.</returns>
        abstract eigs: x: MathCollection * ?prec: U2<float, BigNumber> -> {| values: MathCollection; vectors: MathCollection |}
        /// <summary>
        /// Compute the matrix exponential, expm(A) = e^A. The matrix must be
        /// square. Not to be confused with exp(a), which performs element-wise
        /// exponentiation. The exponential is calculated using the Padé
        /// approximant with scaling and squaring; see “Nineteen Dubious Ways to
        /// Compute the Exponential of a Matrix,” by Moler and Van Loan.
        /// </summary>
        /// <param name="x">A square matrix</param>
        /// <returns>The exponential of x</returns>
        abstract expm: x: Matrix -> Matrix
        /// <summary>
        /// Solves the real-valued Sylvester equation AX-XB=C for X, where A, B and C are
        /// matrices of appropriate dimensions, being A and B squared. The method used is
        /// the Bartels-Stewart algorithm.
        /// <see href="https://en.wikipedia.org/wiki/Sylvester_equation" />
        /// </summary>
        /// <param name="A">Matrix A</param>
        /// <param name="B">Matrix B</param>
        /// <param name="C">Matrix C</param>
        /// <returns>Matrix X, solving the Sylvester equation</returns>
        abstract sylvester: A: MathCollection * B: MathCollection * C: MathCollection -> MathCollection
        /// <summary>
        /// Performs a real Schur decomposition of the real matrix A = UTU' where U is orthogonal
        /// and T is upper quasi-triangular.
        /// <see href="https://en.wikipedia.org/wiki/Schur_decomposition" />
        /// </summary>
        /// <param name="A">Matrix A</param>
        /// <returns>Object containing both matrix U and T of the Schur Decomposition A=UTU'</returns>
        abstract schur: A: MathCollection -> SchurDecomposition
        /// <summary>
        /// Solves the Continuous-time Lyapunov equation AP+PA'=Q for P, where Q is a positive semidefinite
        /// matrix.
        /// <see href="https://en.wikipedia.org/wiki/Lyapunov_equation" />
        /// </summary>
        /// <param name="A">Matrix A</param>
        /// <param name="Q">Matrix Q</param>
        /// <returns>Matrix P solution to the Continuous-time Lyapunov equation AP+PA'=Q</returns>
        abstract lyap: A: MathCollection * Q: MathCollection -> MathCollection
        /// <summary>
        /// Create a 2-dimensional identity matrix with size m x n or n x n. The
        /// matrix has ones on the diagonal and zeros elsewhere.
        /// </summary>
        /// <param name="size">The size for the matrix</param>
        /// <param name="format">The Matrix storage format</param>
        /// <returns>A matrix with ones on the diagonal</returns>
        abstract identity: size: U3<float, ResizeArray<float>, MathCollection> * ?format: string -> U2<MathCollection, float>
        /// <param name="m">The x dimension for the matrix</param>
        /// <param name="n">The y dimension for the matrix</param>
        /// <param name="format">The Matrix storage format</param>
        /// <returns>A matrix with ones on the diagonal</returns>
        abstract identity: m: float * n: float * ?format: string -> U2<MathCollection, float>
        /// <summary>Filter the items in an array or one dimensional matrix.</summary>
        /// <param name="x">A one dimensional matrix or array to filter</param>
        /// <param name="test">
        /// A function or regular expression to test items. All
        /// entries for which test returns true are returned. When test is a
        /// function, it is invoked with three parameters: the value of the
        /// element, the index of the element, and the matrix/array being
        /// traversed. The function must return a boolean.
        /// </param>
        abstract filter: x: U2<MathCollection, ResizeArray<string>> * test: U2<(obj option -> obj option -> U2<MathCollection, ResizeArray<string>> -> bool), RegExp> -> MathCollection
        /// <summary>Flatten a multi dimensional matrix into a single dimensional matrix.</summary>
        /// <param name="x">Matrix to be flattened</param>
        /// <returns>Returns the flattened matrix</returns>
        abstract flatten: x: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Iterate over all elements of a matrix/array, and executes the given
        /// callback function.
        /// </summary>
        /// <param name="x">The matrix to iterate on.</param>
        /// <param name="callback">
        /// The callback function is invoked with three
        /// parameters: the value of the element, the index of the element, and
        /// the Matrix/array being traversed.
        /// </param>
        abstract forEach: x: 'T * callback: (obj option -> obj option -> 'T -> unit) -> unit when 'T :> MathCollection
        /// <summary>Calculate the inverse of a square matrix.</summary>
        /// <param name="x">Matrix to be inversed</param>
        /// <returns>The inverse of x</returns>
        abstract inv: x: 'T -> NoLiteralType<'T>
        /// <summary>Calculate the kronecker product of two matrices or vectors</summary>
        /// <param name="x">First vector</param>
        /// <param name="y">Second vector</param>
        /// <returns>Returns the kronecker product of x and y</returns>
        abstract kron: x: MathCollection * y: MathCollection -> Matrix
        /// <summary>
        /// Iterate over all elements of a matrix/array, and executes the given
        /// callback function.
        /// </summary>
        /// <param name="x">The matrix to iterate on.</param>
        /// <param name="callback">
        /// The callback function is invoked with three
        /// parameters: the value of the element, the index of the element, and
        /// the Matrix/array being traversed.
        /// </param>
        /// <returns>Transformed map of x</returns>
        abstract map: x: 'T * callback: (obj option -> obj option -> 'T -> U2<MathType, string>) -> 'T when 'T :> MathCollection
        /// <summary>
        /// Create a matrix filled with ones. The created matrix can have one or
        /// multiple dimensions.
        /// </summary>
        /// <param name="size">The size of each dimension of the matrix</param>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with ones</returns>
        abstract ones: ?size: U4<float, ResizeArray<float>, BigNumber, ResizeArray<BigNumber>> * ?format: string -> MathCollection
        /// <param name="m">The x dimension of the matrix</param>
        /// <param name="n">The y dimension of the matrix</param>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with ones</returns>
        abstract ones: m: U2<float, BigNumber> * n: U2<float, BigNumber> * ?format: string -> MathCollection
        /// <param name="m">The x dimension of the matrix</param>
        /// <param name="n">The y dimension of the matrix</param>
        /// <param name="p">The z dimension of the matrix</param>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with ones</returns>
        abstract ones: m: U2<float, BigNumber> * n: U2<float, BigNumber> * p: U2<float, BigNumber> * ?format: string -> MathCollection
        /// <summary>
        /// Partition-based selection of an array or 1D matrix. Will find the kth
        /// smallest value, and mutates the input array. Uses Quickselect.
        /// </summary>
        /// <param name="x">A one dimensional matrix or array to sort</param>
        /// <param name="k">The kth smallest value to be retrieved; zero-based index</param>
        /// <param name="compare">
        /// An optional comparator function. The function is
        /// called as compare(a, b), and must return 1 when a &gt; b, -1 when a &lt; b,
        /// and 0 when a == b. Default value: 'asc'.
        /// </param>
        /// <returns>Returns the kth lowest value.</returns>
        abstract partitionSelect: x: MathCollection * k: float * ?compare: U2<(obj option -> obj option -> float), string> -> obj option
        /// <summary>Calculate the Moore–Penrose inverse of a matrix.</summary>
        /// <param name="x">Matrix to be inversed</param>
        /// <returns>The inverse of <c>x</c>.</returns>
        abstract pinv: x: 'T -> 'T when 'T :> MathType
        /// <summary>
        /// Create an array from a range. By default, the range end is excluded.
        /// This can be customized by providing an extra parameter includeEnd.
        /// </summary>
        /// <param name="str">A string 'start:end' or 'start:step:end'</param>
        /// <param name="start">Start of the range</param>
        /// <param name="end">
        /// End of the range, excluded by default, included when
        /// parameter includeEnd=true
        /// </param>
        /// <param name="step">Step size. Default value is 1.</param>
        /// <param name="includeEnd">
        /// Option to specify whether to include the end or
        /// not. False by default
        /// </param>
        /// <returns>
        /// Parameters describing the ranges start, end, and optional
        /// step.
        /// </returns>
        abstract range: str: string * ?includeEnd: bool -> Matrix
        abstract range: start: U2<float, BigNumber> * ``end``: U2<float, BigNumber> * ?includeEnd: bool -> Matrix
        abstract range: start: U2<float, BigNumber> * ``end``: U2<float, BigNumber> * step: U2<float, BigNumber> * ?includeEnd: bool -> Matrix
        /// <summary>Reshape a multi dimensional array to fit the specified dimensions</summary>
        /// <param name="x">Matrix to be reshaped</param>
        /// <param name="sizes">
        /// One dimensional array with integral sizes for each
        /// dimension
        /// </param>
        /// <returns>A reshaped clone of matrix x</returns>
        abstract reshape: x: 'T * sizes: ResizeArray<float> -> 'T when 'T :> MathCollection
        /// <summary>Resize a matrix</summary>
        /// <param name="x">Matrix to be resized</param>
        /// <param name="size">One dimensional array with numbers</param>
        /// <param name="defaultValue">
        /// Zero by default, except in case of a string, in
        /// that case defaultValue = ' ' Default value: 0.
        /// </param>
        /// <returns>A resized clone of matrix x</returns>
        abstract resize: x: 'T * size: MathCollection * ?defaultValue: U2<float, string> -> 'T when 'T :> MathCollection
        /// <summary>Return a Rotation Matrix for a given angle in radians</summary>
        /// <param name="theta">Rotation angle</param>
        /// <param name="v">Rotation axis</param>
        /// <param name="format">Result Matrix storage format. Default value: 'dense'.</param>
        /// <returns>Rotation Matrix</returns>
        abstract rotationMatrix: ?theta: U4<float, BigNumber, Complex, Unit> * ?axis: 'T * ?format: MathJsStaticMatrix -> 'T when 'T :> MathCollection
        /// <summary>Return a row from a Matrix.</summary>
        /// <param name="value">An array or matrix</param>
        /// <param name="row">The index of the row</param>
        /// <returns>The retrieved row</returns>
        abstract row: value: 'T * row: float -> 'T when 'T :> MathCollection
        /// <summary>Return a column from a Matrix.</summary>
        /// <param name="value">An array or matrix</param>
        /// <param name="column">The index of the column</param>
        /// <returns>The retrieved column</returns>
        abstract column: value: 'T * column: float -> 'T when 'T :> MathCollection
        /// <summary>Return a rotated matrix.</summary>
        /// <param name="w">Vector to rotate</param>
        /// <param name="theta">Rotation angle</param>
        /// <param name="v">Rotation axis</param>
        /// <returns>Multiplication of the rotation matrix and w</returns>
        abstract rotate: w: 'T * theta: U4<float, BigNumber, Complex, Unit> * ?v: 'T -> 'T when 'T :> MathCollection
        /// <summary>Calculate the size of a matrix or scalar.</summary>
        /// <param name="A">matrix</param>
        /// <returns>A vector with the size of x</returns>
        abstract size: x: U6<bool, float, Complex, Unit, string, MathCollection> -> MathCollection
        /// <summary>Sort the items in a matrix</summary>
        /// <param name="x">A one dimensional matrix or array to sort</param>
        /// <param name="compare">
        /// An optional _comparator function or name. The function
        /// is called as compare(a, b), and must return 1 when a &gt; b, -1 when a &lt;
        /// b, and 0 when a == b. Default value: ‘asc’
        /// </param>
        /// <returns>Returns the sorted matrix</returns>
        abstract sort: x: 'T * compare: U2<(obj option -> obj option -> float), string> -> 'T when 'T :> MathCollection
        /// <summary>
        /// Calculate the principal square root of a square matrix. The principal
        /// square root matrix X of another matrix A is such that X * X = A.
        /// </summary>
        /// <param name="A">The square matrix A</param>
        /// <returns>The principal square root of matrix A</returns>
        abstract sqrtm: A: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Squeeze a matrix, remove inner and outer singleton dimensions from a
        /// matrix.
        /// </summary>
        /// <param name="x">Matrix to be squeezed</param>
        /// <returns>Squeezed matrix</returns>
        abstract squeeze: x: 'T -> 'T when 'T :> MathCollection
        /// <summary>Get or set a subset of a matrix or string.</summary>
        /// <param name="value">An array, matrix, or string</param>
        /// <param name="index">For each dimension, an index or list of indices to get or set.</param>
        /// <param name="replacement">
        /// An array, matrix, or scalar. If provided, the
        /// subset is replaced with replacement. If not provided, the subset is
        /// returned
        /// </param>
        /// <param name="defaultValue">
        /// Default value, filled in on new entries when the
        /// matrix is resized. If not provided, math.matrix elements will be left
        /// undefined. Default value: undefined.
        /// </param>
        /// <returns>Either the retrieved subset or the updated matrix</returns>
        abstract subset: value: 'T * index: Index * ?replacement: obj * ?defaultValue: obj -> 'T
        /// <summary>
        /// Calculate the trace of a matrix: the sum of the elements on the main
        /// diagonal of a square matrix.
        /// </summary>
        /// <param name="x">A matrix</param>
        /// <returns>The trace of x</returns>
        abstract trace: x: MathCollection -> float
        /// <summary>
        /// Transpose a matrix. All values of the matrix are reflected over its
        /// main diagonal. Only two dimensional matrices are supported.
        /// </summary>
        /// <param name="x">Matrix to be transposed</param>
        /// <returns>The transposed matrix</returns>
        abstract transpose: x: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Create a matrix filled with zeros. The created matrix can have one or
        /// multiple dimensions.
        /// </summary>
        /// <param name="size">The size of each dimension of the matrix</param>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with zeros</returns>
        abstract zeros: ?size: U4<float, ResizeArray<float>, BigNumber, ResizeArray<BigNumber>> * ?format: string -> MathCollection
        /// <param name="m">The x dimension of the matrix</param>
        /// <param name="n">The y dimension of the matrix</param>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with zeros</returns>
        abstract zeros: m: U2<float, BigNumber> * n: U2<float, BigNumber> * ?format: string -> MathCollection
        /// <param name="m">The x dimension of the matrix</param>
        /// <param name="n">The y dimension of the matrix</param>
        /// <param name="p">The z dimension of the matrix</param>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with zeros</returns>
        abstract zeros: m: U2<float, BigNumber> * n: U2<float, BigNumber> * p: U2<float, BigNumber> * ?format: string -> MathCollection
        /// <summary>Calculate N-dimensional fourier transform</summary>
        /// <param name="arr">An array or matrix</param>
        /// <returns>N-dimensional fourier transformation of the array</returns>
        abstract fft: arr: 'T -> 'T when 'T :> MathCollection
        /// <summary>Calculate N-dimensional inverse fourier transform</summary>
        /// <param name="arr">An array or matrix</param>
        /// <returns>N-dimensional fourier transformation of the array</returns>
        abstract ifft: arr: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Compute the number of ways of picking k unordered outcomes from n
        /// possibilities. Combinations only takes integer arguments. The
        /// following condition must be enforced: k &lt;= n.
        /// </summary>
        /// <param name="n">Total number of objects in the set</param>
        /// <param name="k">Number of objects in the subset</param>
        /// <returns>Number of possible combinations</returns>
        abstract combinations: n: 'T * k: U2<float, BigNumber> -> NoLiteralType<'T>
        /// <summary>
        /// Compute the factorial of a value Factorial only supports an integer
        /// value as argument. For matrices, the function is evaluated element
        /// wise.
        /// </summary>
        /// <param name="n">An integer number</param>
        /// <returns>The factorial of n</returns>
        abstract factorial: n: 'T -> NoLiteralType<'T>
        /// <summary>
        /// Compute the gamma function of a value using Lanczos approximation for
        /// small values, and an extended Stirling approximation for large
        /// values.
        /// </summary>
        /// <param name="n">A real or complex number</param>
        /// <returns>The gamma of n</returns>
        abstract gamma: n: 'T -> NoLiteralType<'T>
        /// <summary>
        /// Calculate the Kullback-Leibler (KL) divergence between two
        /// distributions
        /// </summary>
        /// <param name="q">First vector</param>
        /// <param name="p">Second vector</param>
        /// <returns>Returns disance between q and p</returns>
        abstract kldivergence: q: MathCollection * p: MathCollection -> float
        /// <summary>Compute the log gamma function of a value, using Lanczos approximation for numbers and Stirling series for complex numbers.</summary>
        /// <param name="n">A real or complex number</param>
        /// <returns>The log gamma of <c>n</c></returns>
        abstract lgamma: n: 'T -> NoLiteralType<'T>
        /// <summary>
        /// Multinomial Coefficients compute the number of ways of picking a1,
        /// a2, ..., ai unordered outcomes from n possibilities. multinomial
        /// takes one array of integers as an argument. The following condition
        /// must be enforced: every ai &lt;= 0
        /// </summary>
        /// <param name="a">Integer number of objects in the subset</param>
        /// <returns>multinomial coefficent</returns>
        abstract multinomial: a: ResizeArray<'T> -> NoLiteralType<'T>
        /// <summary>
        /// Compute the number of ways of obtaining an ordered subset of k
        /// elements from a set of n elements. Permutations only takes integer
        /// arguments. The following condition must be enforced: k &lt;= n.
        /// </summary>
        /// <param name="n">The number of objects in total</param>
        /// <param name="k">The number of objects in the subset</param>
        /// <returns>The number of permutations</returns>
        abstract permutations: n: 'T * ?k: U2<float, BigNumber> -> NoLiteralType<'T>
        /// <summary>
        /// Random pick a value from a one dimensional array. Array element is
        /// picked using a random function with uniform distribution.
        /// </summary>
        /// <param name="array">A one dimensional array</param>
        /// <param name="number">An int or float</param>
        /// <param name="weights">An array of ints or floats</param>
        /// <returns>
        /// Returns a single random value from array when number is 1 or
        /// undefined. Returns an array with the configured number of elements
        /// when number is &gt; 1.
        /// </returns>
        abstract pickRandom: array: ResizeArray<'T> -> 'T
        abstract pickRandom: array: ResizeArray<'T> * number: float -> ResizeArray<'T>
        abstract pickRandom: array: ResizeArray<'T> * number: float * weights: ResizeArray<float> -> ResizeArray<'T>
        /// <summary>
        /// Return a random number larger or equal to min and smaller than max
        /// using a uniform distribution.
        /// </summary>
        /// <param name="size">
        /// If provided, an array or matrix with given size and
        /// filled with random values is returned
        /// </param>
        /// <param name="min">Minimum boundary for the random value, included</param>
        /// <param name="max">Maximum boundary for the random value, excluded</param>
        /// <returns>A random number</returns>
        abstract random: ?min: float * ?max: float -> float
        abstract random: size: 'T * ?min: float * ?max: float -> 'T when 'T :> MathCollection
        /// <summary>
        /// Return a random integer number larger or equal to min and smaller
        /// than max using a uniform distribution.
        /// </summary>
        /// <param name="size">
        /// If provided, an array or matrix with given size and
        /// filled with random values is returned
        /// </param>
        /// <param name="min">Minimum boundary for the random value, included</param>
        /// <param name="max">Maximum boundary for the random value, excluded</param>
        /// <returns>A random number</returns>
        abstract randomInt: min: float * ?max: float -> float
        abstract randomInt: size: 'T * ?min: float * ?max: float -> 'T when 'T :> MathCollection
        /// <summary>
        /// Compare two values. Returns 1 when x &gt; y, -1 when x &lt; y, and 0 when x
        /// == y. x and y are considered equal when the relative difference
        /// between x and y is smaller than the configured epsilon. The function
        /// cannot be used to compare values smaller than approximately 2.22e-16.
        /// For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to compare</param>
        /// <returns>
        /// Returns the result of the comparison: 1 when x &gt; y, -1 when
        /// x &lt; y, and 0 when x == y.
        /// </returns>
        abstract compare: x: U2<MathType, string> * y: U2<MathType, string> -> U4<float, BigNumber, Fraction, MathCollection>
        /// <summary>
        /// Compare two values of any type in a deterministic, natural way. For
        /// numeric values, the function works the same as math.compare. For
        /// types of values that can’t be compared mathematically, the function
        /// compares in a natural way.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to compare</param>
        /// <returns>
        /// Returns the result of the comparison: 1 when x &gt; y, -1 when
        /// x &lt; y, and 0 when x == y.
        /// </returns>
        abstract compareNatural: x: obj option * y: obj option -> float
        /// <summary>
        /// Compare two strings lexically. Comparison is case sensitive. Returns
        /// 1 when x &gt; y, -1 when x &lt; y, and 0 when x == y. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="x">First string to compare</param>
        /// <param name="y">Second string to compare</param>
        /// <returns>
        /// Returns the result of the comparison: 1 when x &gt; y, -1 when
        /// x &lt; y, and 0 when x == y.
        /// </returns>
        abstract compareText: x: U2<string, MathCollection> * y: U2<string, MathCollection> -> U2<float, MathCollection>
        /// <summary>
        /// Test element wise whether two matrices are equal. The function
        /// accepts both matrices and scalar values.
        /// </summary>
        /// <param name="x">First matrix to compare</param>
        /// <param name="y">Second amtrix to compare</param>
        /// <returns>
        /// Returns true when the input matrices have the same size and
        /// each of their elements is equal.
        /// </returns>
        abstract deepEqual: x: MathType * y: MathType -> MathType
        /// <summary>
        /// Test whether two values are equal.
        /// 
        /// The function tests whether the relative difference between x and y is
        /// smaller than the configured epsilon. The function cannot be used to
        /// compare values smaller than approximately 2.22e-16. For matrices, the
        /// function is evaluated element wise. In case of complex numbers, x.re
        /// must equal y.re, and x.im must equal y.im. Values null and undefined
        /// are compared strictly, thus null is only equal to null and nothing
        /// else, and undefined is only equal to undefined and nothing else.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to compare</param>
        /// <returns>
        /// Returns true when the compared values are equal, else
        /// returns false
        /// </returns>
        abstract equal: x: U2<MathType, string> * y: U2<MathType, string> -> U2<bool, MathCollection>
        /// <summary>
        /// Check equality of two strings. Comparison is case sensitive. For
        /// matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">First string to compare</param>
        /// <param name="y">Second string to compare</param>
        /// <returns>Returns true if the values are equal, and false if not.</returns>
        abstract equalText: x: U2<string, MathCollection> * y: U2<string, MathCollection> -> U2<float, MathCollection>
        /// <summary>
        /// Test whether value x is larger than y. The function returns true when
        /// x is larger than y and the relative difference between x and y is
        /// larger than the configured epsilon. The function cannot be used to
        /// compare values smaller than approximately 2.22e-16. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to vcompare</param>
        /// <returns>Returns true when x is larger than y, else returns false</returns>
        abstract larger: x: U2<MathType, string> * y: U2<MathType, string> -> U2<bool, MathCollection>
        /// <summary>
        /// Test whether value x is larger or equal to y. The function returns
        /// true when x is larger than y or the relative difference between x and
        /// y is smaller than the configured epsilon. The function cannot be used
        /// to compare values smaller than approximately 2.22e-16. For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to vcompare</param>
        /// <returns>
        /// Returns true when x is larger than or equal to y, else
        /// returns false
        /// </returns>
        abstract largerEq: x: U2<MathType, string> * y: U2<MathType, string> -> U2<bool, MathCollection>
        /// <summary>
        /// Test whether value x is smaller than y. The function returns true
        /// when x is smaller than y and the relative difference between x and y
        /// is smaller than the configured epsilon. The function cannot be used
        /// to compare values smaller than approximately 2.22e-16. For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to vcompare</param>
        /// <returns>Returns true when x is smaller than y, else returns false</returns>
        abstract smaller: x: U2<MathType, string> * y: U2<MathType, string> -> U2<bool, MathCollection>
        /// <summary>
        /// Test whether value x is smaller or equal to y. The function returns
        /// true when x is smaller than y or the relative difference between x
        /// and y is smaller than the configured epsilon. The function cannot be
        /// used to compare values smaller than approximately 2.22e-16. For
        /// matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to vcompare</param>
        /// <returns>
        /// Returns true when x is smaller than or equal to y, else
        /// returns false
        /// </returns>
        abstract smallerEq: x: U2<MathType, string> * y: U2<MathType, string> -> U2<bool, MathCollection>
        /// <summary>
        /// Test whether two values are unequal. The function tests whether the
        /// relative difference between x and y is larger than the configured
        /// epsilon. The function cannot be used to compare values smaller than
        /// approximately 2.22e-16. For matrices, the function is evaluated
        /// element wise. In case of complex numbers, x.re must unequal y.re, or
        /// x.im must unequal y.im. Values null and undefined are compared
        /// strictly, thus null is unequal with everything except null, and
        /// undefined is unequal with everything except undefined.
        /// </summary>
        /// <param name="x">First value to compare</param>
        /// <param name="y">Second value to vcompare</param>
        /// <returns>
        /// Returns true when the compared values are unequal, else
        /// returns false
        /// </returns>
        abstract unequal: x: U2<MathType, string> * y: U2<MathType, string> -> U2<bool, MathCollection>
        /// <summary>
        /// Create the cartesian product of two (multi)sets. Multi-dimension
        /// arrays will be converted to single-dimension arrays and the values
        /// will be sorted in ascending order before the operation.
        /// </summary>
        /// <param name="a1">A (multi)set</param>
        /// <param name="a2">A (multi)set</param>
        /// <returns>The cartesian product of two (multi)sets</returns>
        abstract setCartesian: a1: 'T * a2: MathCollection -> 'T when 'T :> MathCollection
        /// <summary>
        /// Create the difference of two (multi)sets: every element of set1, that
        /// is not the element of set2. Multi-dimension arrays will be converted
        /// to single-dimension arrays before the operation
        /// </summary>
        /// <param name="a1">A (multi)set</param>
        /// <param name="a2">A (multi)set</param>
        /// <returns>The difference of two (multi)sets</returns>
        abstract setDifference: a1: 'T * a2: MathCollection -> 'T when 'T :> MathCollection
        /// <summary>
        /// Collect the distinct elements of a multiset. A multi-dimension array
        /// will be converted to a single-dimension array before the operation.
        /// </summary>
        /// <param name="a">A multiset</param>
        /// <returns>A set containing the distinct elements of the multiset</returns>
        abstract setDistinct: a: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Create the intersection of two (multi)sets. Multi-dimension arrays
        /// will be converted to single-dimension arrays before the operation.
        /// </summary>
        /// <param name="a1">A (multi)set</param>
        /// <param name="a2">A (multi)set</param>
        /// <returns>The intersection of two (multi)sets</returns>
        abstract setIntersect: a1: 'T * a2: MathCollection -> 'T when 'T :> MathCollection
        /// <summary>
        /// Check whether a (multi)set is a subset of another (multi)set. (Every
        /// element of set1 is the element of set2.) Multi-dimension arrays will
        /// be converted to single-dimension arrays before the operation.
        /// </summary>
        /// <param name="a1">A (multi)set</param>
        /// <param name="a2">A (multi)set</param>
        /// <returns>True if a1 is subset of a2, else false</returns>
        abstract setIsSubset: a1: MathCollection * a2: MathCollection -> bool
        /// <summary>
        /// Count the multiplicity of an element in a multiset. A multi-dimension
        /// array will be converted to a single-dimension array before the
        /// operation.
        /// </summary>
        /// <param name="e">An element in the multiset</param>
        /// <param name="a">A multiset</param>
        /// <returns>
        /// The number of how many times the multiset contains the
        /// element
        /// </returns>
        abstract setMultiplicity: e: MathNumericType * a: MathCollection -> float
        /// <summary>
        /// Create the powerset of a (multi)set. (The powerset contains very
        /// possible subsets of a (multi)set.) A multi-dimension array will be
        /// converted to a single-dimension array before the operation.
        /// </summary>
        /// <param name="a">A multiset</param>
        /// <returns>The powerset of the (multi)set</returns>
        abstract setPowerset: a: 'T -> 'T when 'T :> MathCollection
        /// <summary>
        /// Count the number of elements of a (multi)set. When a second parameter
        /// is ‘true’, count only the unique values. A multi-dimension array will
        /// be converted to a single-dimension array before the operation.
        /// </summary>
        /// <param name="a">A multiset</param>
        /// <returns>The number of elements of the (multi)set</returns>
        abstract setSize: a: MathCollection -> float
        /// <summary>
        /// Create the symmetric difference of two (multi)sets. Multi-dimension
        /// arrays will be converted to single-dimension arrays before the
        /// operation.
        /// </summary>
        /// <param name="a1">A (multi)set</param>
        /// <param name="a2">A (multi)set</param>
        /// <returns>The symmetric difference of two (multi)sets</returns>
        abstract setSymDifference: a1: 'T * a2: MathCollection -> 'T when 'T :> MathCollection
        /// <summary>
        /// Create the union of two (multi)sets. Multi-dimension arrays will be
        /// converted to single-dimension arrays before the operation.
        /// </summary>
        /// <param name="a1">A (multi)set</param>
        /// <param name="a2">A (multi)set</param>
        /// <returns>The union of two (multi)sets</returns>
        abstract setUnion: a1: 'T * a2: MathCollection -> 'T when 'T :> MathCollection
        /// <summary>
        /// Compute the erf function of a value using a rational Chebyshev
        /// approximations for different intervals of x.
        /// </summary>
        /// <param name="x">A real number</param>
        /// <returns>The erf of x</returns>
        abstract erf: x: 'T -> NoLiteralType<'T>
        /// <summary>
        /// Compute the median absolute deviation of a matrix or a list with
        /// values. The median absolute deviation is defined as the median of the
        /// absolute deviations from the median.
        /// </summary>
        /// <param name="array">A single matrix or multiple scalar values.</param>
        /// <returns>The median absolute deviation</returns>
        abstract mad: array: MathCollection -> obj option
        /// <summary>
        /// Compute the maximum value of a matrix or a list with values. In case
        /// of a multi dimensional array, the maximum of the flattened array will
        /// be calculated. When dim is provided, the maximum over the selected
        /// dimension will be calculated. Parameter dim is zero-based.
        /// </summary>
        /// <param name="args">A single matrix or multiple scalar values</param>
        /// <returns>The maximum value</returns>
        abstract max: [<ParamArray>] args: MathType[] -> obj option
        /// <param name="A">A single matrix</param>
        /// <param name="dim">The maximum over the selected dimension</param>
        /// <returns>The maximum value</returns>
        abstract max: A: MathCollection * ?dim: float -> obj option
        /// <summary>
        /// Compute the mean value of matrix or a list with values. In case of a
        /// multi dimensional array, the mean of the flattened array will be
        /// calculated. When dim is provided, the maximum over the selected
        /// dimension will be calculated. Parameter dim is zero-based.
        /// </summary>
        /// <param name="args">A single matrix or multiple scalar values</param>
        /// <returns>The mean of all values</returns>
        abstract mean: [<ParamArray>] args: MathType[] -> obj option
        /// <param name="A">A single matrix</param>
        /// <param name="dim">The mean over the selected dimension</param>
        /// <returns>The mean of all values</returns>
        abstract mean: A: MathCollection * ?dim: float -> obj option
        /// <summary>
        /// Compute the median of a matrix or a list with values. The values are
        /// sorted and the middle value is returned. In case of an even number of
        /// values, the average of the two middle values is returned. Supported
        /// types of values are: Number, BigNumber, Unit In case of a (multi
        /// dimensional) array or matrix, the median of all elements will be
        /// calculated.
        /// </summary>
        /// <param name="args">A single matrix or or multiple scalar values</param>
        /// <returns>The median</returns>
        abstract median: [<ParamArray>] args: MathType[] -> obj option
        /// <summary>
        /// Compute the minimum value of a matrix or a list of values. In case of
        /// a multi dimensional array, the minimun of the flattened array will be
        /// calculated. When dim is provided, the minimun over the selected
        /// dimension will be calculated. Parameter dim is zero-based.
        /// </summary>
        /// <param name="args">A single matrix or or multiple scalar values</param>
        /// <returns>The minimum value</returns>
        abstract min: [<ParamArray>] args: MathType[] -> obj option
        /// <param name="A">A single matrix</param>
        /// <param name="dim">The minimum over the selected dimension</param>
        /// <returns>The minimum value</returns>
        abstract min: A: MathCollection * ?dim: float -> obj option
        /// <summary>
        /// Computes the mode of a set of numbers or a list with values(numbers
        /// or characters). If there are more than one modes, it returns a list
        /// of those values.
        /// </summary>
        /// <param name="args">A single matrix</param>
        /// <returns>The mode of all values</returns>
        abstract mode: [<ParamArray>] args: MathType[] -> obj option
        /// <summary>
        /// Compute the product of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the sum of all elements will be
        /// calculated.
        /// </summary>
        /// <param name="args">A single matrix or multiple scalar values</param>
        /// <returns>The product of all values</returns>
        abstract prod: [<ParamArray>] args: MathType[] -> obj option
        /// <summary>
        /// Compute the prob order quantile of a matrix or a list with values.
        /// The sequence is sorted and the middle value is returned. Supported
        /// types of sequence values are: Number, BigNumber, Unit Supported types
        /// of probability are: Number, BigNumber In case of a (multi
        /// dimensional) array or matrix, the prob order quantile of all elements
        /// will be calculated.
        /// </summary>
        /// <param name="A">A single matrix or array</param>
        /// <param name="probOrN">
        /// prob is the order of the quantile, while N is the
        /// amount of evenly distributed steps of probabilities; only one of
        /// these options can be provided
        /// </param>
        /// <param name="sorted">=false is data sorted in ascending order</param>
        /// <returns>Quantile(s)</returns>
        abstract quantileSeq: A: MathCollection * prob: U3<float, BigNumber, MathArray> * ?sorted: bool -> U4<float, BigNumber, Unit, MathArray>
        /// <summary>
        /// Compute the standard deviation of a matrix or a list with values. The
        /// standard deviations is defined as the square root of the variance:
        /// std(A) = sqrt(variance(A)). In case of a (multi dimensional) array or
        /// matrix, the standard deviation over all elements will be calculated.
        /// Optionally, the type of normalization can be specified as second
        /// parameter. The parameter normalization can be one of the following
        /// values: 'unbiased' (default) The sum of squared errors is divided by
        /// (n - 1) 'uncorrected' The sum of squared errors is divided by n
        /// 'biased' The sum of squared errors is divided by (n + 1)
        /// </summary>
        /// <param name="a">variadic argument of number to calculate standard deviation</param>
        /// <returns>The standard deviation array</returns>
        abstract std: [<ParamArray>] values: float[] -> float
        /// <summary>
        /// Compute the standard deviation of a matrix or a list with values. The
        /// standard deviations is defined as the square root of the variance:
        /// std(A) = sqrt(variance(A)). In case of a (multi dimensional) array or
        /// matrix, the standard deviation over all elements will be calculated.
        /// Optionally, the type of normalization can be specified as second
        /// parameter. The parameter normalization can be one of the following
        /// values: 'unbiased' (default) The sum of squared errors is divided by
        /// (n - 1) 'uncorrected' The sum of squared errors is divided by n
        /// 'biased' The sum of squared errors is divided by (n + 1)
        /// </summary>
        /// <param name="array">A single matrix to compute standard deviation.</param>
        /// <param name="dimension">A dimension to calculate standard deviation</param>
        /// <param name="normalization">
        /// Determines how to normalize the variance. Choose
        /// ‘unbiased’ (default), ‘uncorrected’, or ‘biased’. Default value:
        /// ‘unbiased’.
        /// </param>
        /// <returns>The standard deviation array</returns>
        abstract std: array: MathCollection * ?dimension: float * ?normalization: MathJsStaticStd -> ResizeArray<float>
        /// <summary>
        /// Compute the standard deviation of a matrix or a list with values. The
        /// standard deviations is defined as the square root of the variance:
        /// std(A) = sqrt(variance(A)). In case of a (multi dimensional) array or
        /// matrix, the standard deviation over all elements will be calculated.
        /// Optionally, the type of normalization can be specified as second
        /// parameter. The parameter normalization can be one of the following
        /// values: 'unbiased' (default) The sum of squared errors is divided by
        /// (n - 1) 'uncorrected' The sum of squared errors is divided by n
        /// 'biased' The sum of squared errors is divided by (n + 1)
        /// </summary>
        /// <param name="array">A single matrix or multiple scalar values</param>
        /// <param name="normalization">
        /// Determines how to normalize the variance. Choose
        /// ‘unbiased’ (default), ‘uncorrected’, or ‘biased’. Default value:
        /// ‘unbiased’.
        /// </param>
        /// <returns>The standard deviation</returns>
        abstract std: array: MathCollection * normalization: MathJsStaticStd -> float
        /// <summary>
        /// Compute the sum of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the sum of all elements will be
        /// calculated.
        /// </summary>
        /// <param name="args">A single matrix or multiple scalar values</param>
        /// <returns>The sum of all values</returns>
        abstract sum: [<ParamArray>] args: Array<U3<float, BigNumber, Fraction>> -> obj option
        /// <param name="array">A single matrix</param>
        /// <returns>The sum of all values</returns>
        abstract sum: array: MathCollection -> obj option
        /// <summary>Count the number of elements of a matrix, array or string.</summary>
        /// <param name="x">A matrix, array or string.</param>
        /// <returns>The number of members passed in parameters</returns>
        abstract count: x: U2<MathCollection, string> -> float
        /// <summary>
        /// Compute the cumulative sum of a matrix or a list with values.
        /// In case of a (multi dimensional) array or matrix, the cumulative sums
        /// along a specified dimension (defaulting to the first) will be calculated.
        /// </summary>
        /// <param name="args">A single matrix or multiple scalar values</param>
        /// <returns>The cumulative sums of the the values.</returns>
        abstract cumsum: [<ParamArray>] args: MathType[] -> ResizeArray<MathType>
        /// <param name="array">A single matrix</param>
        /// <param name="dim">The dimension along which to sum (defaults to 0)</param>
        /// <returns>The cumulative sums along the given dimension</returns>
        abstract cumsum: array: MathCollection * ?dim: float -> MathCollection
        /// <summary>
        /// Compute the variance of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the variance over all elements
        /// will be calculated. Optionally, the type of normalization can be
        /// specified as second parameter. The parameter normalization can be one
        /// of the following values: 'unbiased' (default) The sum of squared
        /// errors is divided by (n - 1) 'uncorrected' The sum of squared errors
        /// is divided by n 'biased' The sum of squared errors is divided by (n +
        /// 1) Note that older browser may not like the variable name var. In
        /// that case, the function can be called as math<see cref="...">'var'</see> instead of
        /// math.variance(...).
        /// </summary>
        /// <param name="args">A single matrix or multiple scalar values</param>
        /// <returns>The variance</returns>
        abstract variance: [<ParamArray>] args: Array<U3<float, BigNumber, Fraction>> -> float
        /// <summary>
        /// Compute the variance of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the variance over all elements
        /// will be calculated. Optionally, the type of normalization can be
        /// specified as second parameter. The parameter normalization can be one
        /// of the following values: 'unbiased' (default) The sum of squared
        /// errors is divided by (n - 1) 'uncorrected' The sum of squared errors
        /// is divided by n 'biased' The sum of squared errors is divided by (n +
        /// 1) Note that older browser may not like the variable name var. In
        /// that case, the function can be called as math<see cref="...">'var'</see> instead of
        /// math.variance(...).
        /// </summary>
        /// <param name="array">A matrix to compute variance.</param>
        /// <param name="dimension">A dimension to compute variance on</param>
        /// <param name="normalization">
        /// normalization Determines how to normalize the
        /// variance. Choose ‘unbiased’ (default), ‘uncorrected’, or ‘biased’.
        /// Default value: ‘unbiased’.
        /// </param>
        /// <returns>variance matrix.</returns>
        abstract variance: array: MathCollection * ?dimension: float * ?normalization: MathJsStaticStd -> ResizeArray<float>
        /// <param name="array">A single matrix</param>
        /// <param name="normalization">
        /// normalization Determines how to normalize the
        /// variance. Choose ‘unbiased’ (default), ‘uncorrected’, or ‘biased’.
        /// Default value: ‘unbiased’.
        /// </param>
        /// <returns>The variance</returns>
        abstract variance: array: MathCollection * normalization: MathJsStaticStd -> float
        /// <summary>Format a value of any type into a string.</summary>
        /// <param name="value">The value to be formatted</param>
        /// <param name="options">An object with formatting options.</param>
        /// <param name="callback">
        /// A custom formatting function, invoked for all numeric
        /// elements in value, for example all elements of a matrix, or the real
        /// and imaginary parts of a complex number. This callback can be used to
        /// override the built-in numeric notation with any type of formatting.
        /// Function callback is called with value as parameter and must return a
        /// string.
        /// </param>
        /// <seealso href="http://mathjs.org/docs/reference/functions/format.html" />
        /// <returns>The formatted value</returns>
        abstract format: value: obj option * ?options: U3<FormatOptions, float, (obj option -> string)> * ?callback: (obj option -> string) -> string
        /// <summary>Interpolate values into a string template.</summary>
        /// <param name="template">A string containing variable placeholders.</param>
        /// <param name="values">
        /// An object containing variables which will be filled in
        /// in the template.
        /// </param>
        /// <param name="precision">
        /// Number of digits to format numbers. If not provided,
        /// the value will not be rounded.
        /// </param>
        /// <param name="options">
        /// Formatting options, or the number of digits to format
        /// numbers. See function math.format for a description of all options.
        /// </param>
        /// <returns>Interpolated string</returns>
        abstract print: template: string * values: obj option * ?precision: float * ?options: U2<float, obj> -> unit
        /// <summary>Calculate the inverse cosine of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The arc cosine of x</returns>
        abstract acos: x: float -> U2<float, Complex>
        abstract acos: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic arccos of a value, defined as acosh(x) =
        /// ln(sqrt(x^2 - 1) + x).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic arccosine of x</returns>
        abstract acosh: x: float -> U2<float, Complex>
        abstract acosh: x: 'T -> 'T
        /// <summary>Calculate the inverse cotangent of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The arc cotangent of x</returns>
        abstract acot: x: float -> float
        abstract acot: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic arccotangent of a value, defined as acoth(x)
        /// = (ln((x+1)/x) + ln(x/(x-1))) / 2.
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic arccotangent of x</returns>
        abstract acoth: x: float -> float
        abstract acoth: x: 'T -> 'T
        /// <summary>Calculate the inverse cosecant of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The arc cosecant of x</returns>
        abstract acsc: x: float -> U2<float, Complex>
        abstract acsc: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic arccosecant of a value, defined as acsch(x)
        /// = ln(1/x + sqrt(1/x^2 + 1)).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic arccosecant of x</returns>
        abstract acsch: x: float -> float
        abstract acsch: x: 'T -> 'T
        /// <summary>Calculate the inverse secant of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The arc secant of x</returns>
        abstract asec: x: float -> U2<float, Complex>
        abstract asec: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic arcsecant of a value, defined as asech(x) =
        /// ln(sqrt(1/x^2 - 1) + 1/x).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic arcsecant of x</returns>
        abstract asech: x: float -> U2<float, Complex>
        abstract asech: x: 'T -> 'T
        /// <summary>Calculate the inverse sine of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The arc sine of x</returns>
        abstract asin: x: float -> U2<float, Complex>
        abstract asin: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic arcsine of a value, defined as asinh(x) =
        /// ln(x + sqrt(x^2 + 1)).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic arcsine of x</returns>
        abstract asinh: x: 'T -> 'T
        /// <summary>Calculate the inverse tangent of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The arc tangent of x</returns>
        abstract atan: x: 'T -> 'T
        /// <summary>
        /// Calculate the inverse tangent function with two arguments, y/x. By
        /// providing two arguments, the right quadrant of the computed angle can
        /// be determined. For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>Four quadrant inverse tangent</returns>
        abstract atan2: y: 'T * x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic arctangent of a value, defined as atanh(x) =
        /// ln((1 + x)/(1 - x)) / 2.
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic arctangent of x</returns>
        abstract atanh: x: float -> U2<float, Complex>
        abstract atanh: x: 'T -> 'T
        /// <summary>Calculate the cosine of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The cosine of x</returns>
        abstract cos: x: U2<float, Unit> -> float
        abstract cos: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic cosine of a value, defined as cosh(x) = 1/2
        /// * (exp(x) + exp(-x)).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic cosine of x</returns>
        abstract cosh: x: U2<float, Unit> -> float
        abstract cosh: x: 'T -> 'T
        /// <summary>Calculate the cotangent of a value. cot(x) is defined as 1 / tan(x).</summary>
        /// <param name="x">Function input</param>
        /// <returns>The cotangent of x</returns>
        abstract cot: x: U2<float, Unit> -> float
        abstract cot: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic cotangent of a value, defined as coth(x) = 1
        /// / tanh(x).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic cotangent of x</returns>
        abstract coth: x: U2<float, Unit> -> float
        abstract coth: x: 'T -> 'T
        /// <summary>Calculate the cosecant of a value, defined as csc(x) = 1/sin(x).</summary>
        /// <param name="x">Function input</param>
        /// <returns>The cosecant hof x</returns>
        abstract csc: x: U2<float, Unit> -> float
        abstract csc: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic cosecant of a value, defined as csch(x) = 1
        /// / sinh(x).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic cosecant of x</returns>
        abstract csch: x: U2<float, Unit> -> float
        abstract csch: x: 'T -> 'T
        /// <summary>Calculate the secant of a value, defined as sec(x) = 1/cos(x).</summary>
        /// <param name="x">Function input</param>
        /// <returns>The secant of x</returns>
        abstract sec: x: U2<float, Unit> -> float
        abstract sec: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic secant of a value, defined as sech(x) = 1 /
        /// cosh(x).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic secant of x</returns>
        abstract sech: x: U2<float, Unit> -> float
        abstract sech: x: 'T -> 'T
        /// <summary>Calculate the sine of a value.</summary>
        /// <param name="x">Function input</param>
        /// <returns>The sine of x</returns>
        abstract sin: x: U2<float, Unit> -> float
        abstract sin: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic sine of a value, defined as sinh(x) = 1/2 *
        /// (exp(x) - exp(-x)).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic sine of x</returns>
        abstract sinh: x: U2<float, Unit> -> float
        abstract sinh: x: 'T -> 'T
        /// <summary>Calculate the tangent of a value. tan(x) is equal to sin(x) / cos(x).</summary>
        /// <param name="x">Function input</param>
        /// <returns>The tangent of x</returns>
        abstract tan: x: U2<float, Unit> -> float
        abstract tan: x: 'T -> 'T
        /// <summary>
        /// Calculate the hyperbolic tangent of a value, defined as tanh(x) =
        /// (exp(2 * x) - 1) / (exp(2 * x) + 1).
        /// </summary>
        /// <param name="x">Function input</param>
        /// <returns>The hyperbolic tangent of x</returns>
        abstract tanh: x: U2<float, Unit> -> float
        abstract tanh: x: 'T -> 'T
        /// <summary>
        /// Change the unit of a value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">The unit to be converted.</param>
        /// <param name="unit">
        /// New unit. Can be a string like "cm" or a unit without
        /// value.
        /// </param>
        /// <returns>Value with changed, fixed unit</returns>
        abstract ``to``: x: U2<Unit, MathCollection> * unit: U2<Unit, string> -> U2<Unit, MathCollection>
        /// ***********************************************************************
        /// Utils
        /// **********************************************************************
        abstract isNumber: x: obj -> bool
        abstract isBigNumber: x: obj -> bool
        abstract isComplex: x: obj -> bool
        abstract isFraction: x: obj -> bool
        abstract isUnit: x: obj -> bool
        abstract isString: x: obj -> bool
        abstract isArray: obj with get, set
        abstract isMatrix: x: obj -> bool
        abstract isCollection: x: obj -> bool
        abstract isDenseMatrix: x: obj -> bool
        abstract isSparseMatrix: x: obj -> bool
        abstract isRange: x: obj -> bool
        abstract isIndex: x: obj -> bool
        abstract isBoolean: x: obj -> bool
        abstract isResultSet: x: obj -> bool
        abstract isHelp: x: obj -> bool
        abstract isFunction: x: obj -> bool
        abstract isDate: x: obj -> bool
        abstract isRegExp: x: obj -> bool
        abstract isObject: x: obj -> bool
        abstract isNull: x: obj -> bool
        abstract isUndefined: x: obj -> bool
        abstract isAccessorNode: x: obj -> bool
        abstract isArrayNode: x: obj -> bool
        abstract isAssignmentNode: x: obj -> bool
        abstract isBlockNode: x: obj -> bool
        abstract isConditionalNode: x: obj -> bool
        abstract isConstantNode: x: obj -> bool
        abstract isFunctionAssignmentNode: x: obj -> bool
        abstract isFunctionNode: x: obj -> bool
        abstract isIndexNode: x: obj -> bool
        abstract isNode: x: obj -> bool
        abstract isObjectNode: x: obj -> bool
        abstract isOperatorNode: x: obj -> bool
        abstract isParenthesisNode: x: obj -> bool
        abstract isRangeNode: x: obj -> bool
        abstract isRelationalNode: x: obj -> bool
        abstract isSymbolNode: x: obj -> bool
        abstract isChain: x: obj -> bool
        /// <summary>Clone an object.</summary>
        /// <param name="x">Object to be cloned</param>
        /// <returns>A clone of object x</returns>
        abstract clone: x: 'TType -> 'TType
        /// <summary>
        /// Test whether a value is an numeric value. In case of a string,
        ///  true is returned if the string contains a numeric value.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is a number, BigNumber, Fraction, Boolean, or a String containing number.
        /// Returns false for other types.
        /// Throws an error in case of unknown types.
        /// </returns>
        abstract hasNumericValue: x: obj option -> U2<bool, ResizeArray<bool>>
        /// <summary>
        /// Test whether a value is an integer number. The function supports
        /// number, BigNumber, and Fraction. The function is evaluated
        /// element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x contains a numeric, integer value.
        /// Throws an error in case of an unknown data type.
        /// </returns>
        abstract isInteger: x: U4<float, BigNumber, Fraction, MathCollection> -> bool
        /// <summary>
        /// Test whether a value is NaN (not a number). The function supports
        /// types number, BigNumber, Fraction, Unit and Complex. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is NaN. Throws an error in case of an
        /// unknown data type.
        /// </returns>
        abstract isNaN: x: U5<float, BigNumber, Fraction, MathCollection, Unit> -> bool
        /// <summary>
        /// Test whether a value is negative: smaller than zero. The function
        /// supports types number, BigNumber, Fraction, and Unit. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is larger than zero. Throws an error in
        /// case of an unknown data type.
        /// </returns>
        abstract isNegative: x: U5<float, BigNumber, Fraction, MathCollection, Unit> -> bool
        /// <summary>
        /// Test whether a value is an numeric value. The function is evaluated
        /// element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is a number, BigNumber, Fraction, or
        /// boolean. Returns false for other types. Throws an error in case of
        /// unknown types.
        /// </returns>
        abstract isNumeric: x: obj option -> bool
        /// <summary>
        /// Test whether a value is positive: larger than zero. The function
        /// supports types number, BigNumber, Fraction, and Unit. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is larger than zero. Throws an error in
        /// case of an unknown data type.
        /// </returns>
        abstract isPositive: x: U5<float, BigNumber, Fraction, MathCollection, Unit> -> bool
        /// <summary>
        /// Test whether a value is prime: has no divisors other than itself and
        /// one. The function supports type number, bignumber. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is larger than zero. Throws an error in
        /// case of an unknown data type.
        /// </returns>
        abstract isPrime: x: U3<float, BigNumber, MathCollection> -> bool
        /// <summary>
        /// Test whether a value is zero. The function can check for zero for
        /// types number, BigNumber, Fraction, Complex, and Unit. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        /// </summary>
        /// <param name="x">Value to be tested</param>
        /// <returns>
        /// Returns true when x is zero. Throws an error in case of an
        /// unknown data type.
        /// </returns>
        abstract isZero: x: MathType -> bool
        /// <summary>Determine the type of a variable.</summary>
        /// <param name="x">The variable for which to test the type</param>
        /// <returns>
        /// Returns the name of the type. Primitive types are lower
        /// case, non-primitive types are upper-camel-case. For example ‘number’,
        /// ‘string’, ‘Array’, ‘Date’.
        /// </returns>
        abstract typeOf: x: obj option -> string
        /// <summary>
        /// Import functions from an object or a module
        /// To avoid errors when using one of the imported functions extend module like this:
        /// </summary>
        /// <example>
        /// // imported_math_functions.ts
        /// declare module 'mathjs' {
        ///      interface MathJsStatic {
        ///          hello(a: number): number;
        ///      }
        /// }
        /// </example>
        /// <param name="object">An object with functions to be imported.</param>
        /// <param name="options">An object with import options.</param>
        abstract import: object: U2<ImportObject, ResizeArray<ImportObject>> * ?options: ImportOptions -> unit

    /// ***********************************************************************
    /// Factory and Dependencies
    /// **********************************************************************
    type [<AllowNullLiteral>] FactoryDependencies =
        abstract create: (FactoryFunctionMap -> (ConfigOptions) option -> MathJsStatic) with get, set
        abstract factory: (string -> 'TDeps -> (obj -> 'T) -> (obj) option -> FactoryFunction<'T>) with get, set
        abstract all: FactoryFunctionMap with get, set
        abstract typedDependencies: FactoryFunctionMap with get, set
        abstract ResultSetDependencies: FactoryFunctionMap with get, set
        abstract BigNumberDependencies: FactoryFunctionMap with get, set
        abstract ComplexDependencies: FactoryFunctionMap with get, set
        abstract FractionDependencies: FactoryFunctionMap with get, set
        abstract RangeDependencies: FactoryFunctionMap with get, set
        abstract MatrixDependencies: FactoryFunctionMap with get, set
        abstract DenseMatrixDependencies: FactoryFunctionMap with get, set
        abstract cloneDependencies: FactoryFunctionMap with get, set
        abstract isIntegerDependencies: FactoryFunctionMap with get, set
        abstract isNegativeDependencies: FactoryFunctionMap with get, set
        abstract isNumericDependencies: FactoryFunctionMap with get, set
        abstract hasNumericValueDependencies: FactoryFunctionMap with get, set
        abstract isPositiveDependencies: FactoryFunctionMap with get, set
        abstract isZeroDependencies: FactoryFunctionMap with get, set
        abstract isNaNDependencies: FactoryFunctionMap with get, set
        abstract typeOfDependencies: FactoryFunctionMap with get, set
        abstract typeofDependencies: FactoryFunctionMap with get, set
        abstract equalScalarDependencies: FactoryFunctionMap with get, set
        abstract SparseMatrixDependencies: FactoryFunctionMap with get, set
        abstract numberDependencies: FactoryFunctionMap with get, set
        abstract stringDependencies: FactoryFunctionMap with get, set
        abstract booleanDependencies: FactoryFunctionMap with get, set
        abstract bignumberDependencies: FactoryFunctionMap with get, set
        abstract complexDependencies: FactoryFunctionMap with get, set
        abstract fractionDependencies: FactoryFunctionMap with get, set
        abstract matrixDependencies: FactoryFunctionMap with get, set
        abstract splitUnitDependencies: FactoryFunctionMap with get, set
        abstract unaryMinusDependencies: FactoryFunctionMap with get, set
        abstract unaryPlusDependencies: FactoryFunctionMap with get, set
        abstract absDependencies: FactoryFunctionMap with get, set
        abstract applyDependencies: FactoryFunctionMap with get, set
        abstract addScalarDependencies: FactoryFunctionMap with get, set
        abstract cbrtDependencies: FactoryFunctionMap with get, set
        abstract ceilDependencies: FactoryFunctionMap with get, set
        abstract cubeDependencies: FactoryFunctionMap with get, set
        abstract expDependencies: FactoryFunctionMap with get, set
        abstract expm1Dependencies: FactoryFunctionMap with get, set
        abstract fixDependencies: FactoryFunctionMap with get, set
        abstract floorDependencies: FactoryFunctionMap with get, set
        abstract gcdDependencies: FactoryFunctionMap with get, set
        abstract lcmDependencies: FactoryFunctionMap with get, set
        abstract log10Dependencies: FactoryFunctionMap with get, set
        abstract log2Dependencies: FactoryFunctionMap with get, set
        abstract modDependencies: FactoryFunctionMap with get, set
        abstract multiplyScalarDependencies: FactoryFunctionMap with get, set
        abstract multiplyDependencies: FactoryFunctionMap with get, set
        abstract nthRootDependencies: FactoryFunctionMap with get, set
        abstract signDependencies: FactoryFunctionMap with get, set
        abstract sqrtDependencies: FactoryFunctionMap with get, set
        abstract squareDependencies: FactoryFunctionMap with get, set
        abstract subtractDependencies: FactoryFunctionMap with get, set
        abstract xgcdDependencies: FactoryFunctionMap with get, set
        abstract dotMultiplyDependencies: FactoryFunctionMap with get, set
        abstract bitAndDependencies: FactoryFunctionMap with get, set
        abstract bitNotDependencies: FactoryFunctionMap with get, set
        abstract bitOrDependencies: FactoryFunctionMap with get, set
        abstract bitXorDependencies: FactoryFunctionMap with get, set
        abstract argDependencies: FactoryFunctionMap with get, set
        abstract conjDependencies: FactoryFunctionMap with get, set
        abstract imDependencies: FactoryFunctionMap with get, set
        abstract reDependencies: FactoryFunctionMap with get, set
        abstract notDependencies: FactoryFunctionMap with get, set
        abstract orDependencies: FactoryFunctionMap with get, set
        abstract xorDependencies: FactoryFunctionMap with get, set
        abstract concatDependencies: FactoryFunctionMap with get, set
        abstract columnDependencies: FactoryFunctionMap with get, set
        abstract crossDependencies: FactoryFunctionMap with get, set
        abstract diagDependencies: FactoryFunctionMap with get, set
        abstract eyeDependencies: FactoryFunctionMap with get, set
        abstract filterDependencies: FactoryFunctionMap with get, set
        abstract flattenDependencies: FactoryFunctionMap with get, set
        abstract forEachDependencies: FactoryFunctionMap with get, set
        abstract getMatrixDataTypeDependencies: FactoryFunctionMap with get, set
        abstract identityDependencies: FactoryFunctionMap with get, set
        abstract kronDependencies: FactoryFunctionMap with get, set
        abstract mapDependencies: FactoryFunctionMap with get, set
        abstract onesDependencies: FactoryFunctionMap with get, set
        abstract rangeDependencies: FactoryFunctionMap with get, set
        abstract reshapeDependencies: FactoryFunctionMap with get, set
        abstract resizeDependencies: FactoryFunctionMap with get, set
        abstract rowDependencies: FactoryFunctionMap with get, set
        abstract sizeDependencies: FactoryFunctionMap with get, set
        abstract squeezeDependencies: FactoryFunctionMap with get, set
        abstract subsetDependencies: FactoryFunctionMap with get, set
        abstract transposeDependencies: FactoryFunctionMap with get, set
        abstract ctransposeDependencies: FactoryFunctionMap with get, set
        abstract zerosDependencies: FactoryFunctionMap with get, set
        abstract erfDependencies: FactoryFunctionMap with get, set
        abstract modeDependencies: FactoryFunctionMap with get, set
        abstract prodDependencies: FactoryFunctionMap with get, set
        abstract formatDependencies: FactoryFunctionMap with get, set
        abstract printDependencies: FactoryFunctionMap with get, set
        abstract toDependencies: FactoryFunctionMap with get, set
        abstract isPrimeDependencies: FactoryFunctionMap with get, set
        abstract numericDependencies: FactoryFunctionMap with get, set
        abstract divideScalarDependencies: FactoryFunctionMap with get, set
        abstract powDependencies: FactoryFunctionMap with get, set
        abstract roundDependencies: FactoryFunctionMap with get, set
        abstract logDependencies: FactoryFunctionMap with get, set
        abstract log1pDependencies: FactoryFunctionMap with get, set
        abstract nthRootsDependencies: FactoryFunctionMap with get, set
        abstract dotPowDependencies: FactoryFunctionMap with get, set
        abstract dotDivideDependencies: FactoryFunctionMap with get, set
        abstract lsolveDependencies: FactoryFunctionMap with get, set
        abstract usolveDependencies: FactoryFunctionMap with get, set
        abstract leftShiftDependencies: FactoryFunctionMap with get, set
        abstract rightArithShiftDependencies: FactoryFunctionMap with get, set
        abstract rightLogShiftDependencies: FactoryFunctionMap with get, set
        abstract andDependencies: FactoryFunctionMap with get, set
        abstract compareDependencies: FactoryFunctionMap with get, set
        abstract compareNaturalDependencies: FactoryFunctionMap with get, set
        abstract compareTextDependencies: FactoryFunctionMap with get, set
        abstract equalDependencies: FactoryFunctionMap with get, set
        abstract equalTextDependencies: FactoryFunctionMap with get, set
        abstract smallerDependencies: FactoryFunctionMap with get, set
        abstract smallerEqDependencies: FactoryFunctionMap with get, set
        abstract largerDependencies: FactoryFunctionMap with get, set
        abstract largerEqDependencies: FactoryFunctionMap with get, set
        abstract deepEqualDependencies: FactoryFunctionMap with get, set
        abstract unequalDependencies: FactoryFunctionMap with get, set
        abstract partitionSelectDependencies: FactoryFunctionMap with get, set
        abstract sortDependencies: FactoryFunctionMap with get, set
        abstract maxDependencies: FactoryFunctionMap with get, set
        abstract minDependencies: FactoryFunctionMap with get, set
        abstract ImmutableDenseMatrixDependencies: FactoryFunctionMap with get, set
        abstract IndexDependencies: FactoryFunctionMap with get, set
        abstract FibonacciHeapDependencies: FactoryFunctionMap with get, set
        abstract SpaDependencies: FactoryFunctionMap with get, set
        abstract UnitDependencies: FactoryFunctionMap with get, set
        abstract unitDependencies: FactoryFunctionMap with get, set
        abstract sparseDependencies: FactoryFunctionMap with get, set
        abstract createUnitDependencies: FactoryFunctionMap with get, set
        abstract acosDependencies: FactoryFunctionMap with get, set
        abstract acoshDependencies: FactoryFunctionMap with get, set
        abstract acotDependencies: FactoryFunctionMap with get, set
        abstract acothDependencies: FactoryFunctionMap with get, set
        abstract acscDependencies: FactoryFunctionMap with get, set
        abstract acschDependencies: FactoryFunctionMap with get, set
        abstract asecDependencies: FactoryFunctionMap with get, set
        abstract asechDependencies: FactoryFunctionMap with get, set
        abstract asinDependencies: FactoryFunctionMap with get, set
        abstract asinhDependencies: FactoryFunctionMap with get, set
        abstract atanDependencies: FactoryFunctionMap with get, set
        abstract atan2Dependencies: FactoryFunctionMap with get, set
        abstract atanhDependencies: FactoryFunctionMap with get, set
        abstract cosDependencies: FactoryFunctionMap with get, set
        abstract coshDependencies: FactoryFunctionMap with get, set
        abstract cotDependencies: FactoryFunctionMap with get, set
        abstract cothDependencies: FactoryFunctionMap with get, set
        abstract cscDependencies: FactoryFunctionMap with get, set
        abstract cschDependencies: FactoryFunctionMap with get, set
        abstract secDependencies: FactoryFunctionMap with get, set
        abstract sechDependencies: FactoryFunctionMap with get, set
        abstract sinDependencies: FactoryFunctionMap with get, set
        abstract sinhDependencies: FactoryFunctionMap with get, set
        abstract tanDependencies: FactoryFunctionMap with get, set
        abstract tanhDependencies: FactoryFunctionMap with get, set
        abstract setCartesianDependencies: FactoryFunctionMap with get, set
        abstract setDifferenceDependencies: FactoryFunctionMap with get, set
        abstract setDistinctDependencies: FactoryFunctionMap with get, set
        abstract setIntersectDependencies: FactoryFunctionMap with get, set
        abstract setIsSubsetDependencies: FactoryFunctionMap with get, set
        abstract setMultiplicityDependencies: FactoryFunctionMap with get, set
        abstract setPowersetDependencies: FactoryFunctionMap with get, set
        abstract setSizeDependencies: FactoryFunctionMap with get, set
        abstract setSymDifferenceDependencies: FactoryFunctionMap with get, set
        abstract setUnionDependencies: FactoryFunctionMap with get, set
        abstract addDependencies: FactoryFunctionMap with get, set
        abstract hypotDependencies: FactoryFunctionMap with get, set
        abstract normDependencies: FactoryFunctionMap with get, set
        abstract dotDependencies: FactoryFunctionMap with get, set
        abstract traceDependencies: FactoryFunctionMap with get, set
        abstract indexDependencies: FactoryFunctionMap with get, set
        abstract NodeDependencies: FactoryFunctionMap with get, set
        abstract AccessorNodeDependencies: FactoryFunctionMap with get, set
        abstract ArrayNodeDependencies: FactoryFunctionMap with get, set
        abstract AssignmentNodeDependencies: FactoryFunctionMap with get, set
        abstract BlockNodeDependencies: FactoryFunctionMap with get, set
        abstract ConditionalNodeDependencies: FactoryFunctionMap with get, set
        abstract ConstantNodeDependencies: FactoryFunctionMap with get, set
        abstract FunctionAssignmentNodeDependencies: FactoryFunctionMap with get, set
        abstract IndexNodeDependencies: FactoryFunctionMap with get, set
        abstract ObjectNodeDependencies: FactoryFunctionMap with get, set
        abstract OperatorNodeDependencies: FactoryFunctionMap with get, set
        abstract ParenthesisNodeDependencies: FactoryFunctionMap with get, set
        abstract RangeNodeDependencies: FactoryFunctionMap with get, set
        abstract RelationalNodeDependencies: FactoryFunctionMap with get, set
        abstract SymbolNodeDependencies: FactoryFunctionMap with get, set
        abstract FunctionNodeDependencies: FactoryFunctionMap with get, set
        abstract parseDependencies: FactoryFunctionMap with get, set
        abstract compileDependencies: FactoryFunctionMap with get, set
        abstract evaluateDependencies: FactoryFunctionMap with get, set
        abstract evalDependencies: FactoryFunctionMap with get, set
        abstract ParserDependencies: FactoryFunctionMap with get, set
        abstract parserDependencies: FactoryFunctionMap with get, set
        abstract lupDependencies: FactoryFunctionMap with get, set
        abstract qrDependencies: FactoryFunctionMap with get, set
        abstract sluDependencies: FactoryFunctionMap with get, set
        abstract lusolveDependencies: FactoryFunctionMap with get, set
        abstract HelpDependencies: FactoryFunctionMap with get, set
        abstract ChainDependencies: FactoryFunctionMap with get, set
        abstract helpDependencies: FactoryFunctionMap with get, set
        abstract chainDependencies: FactoryFunctionMap with get, set
        abstract detDependencies: FactoryFunctionMap with get, set
        abstract invDependencies: FactoryFunctionMap with get, set
        abstract expmDependencies: FactoryFunctionMap with get, set
        abstract sqrtmDependencies: FactoryFunctionMap with get, set
        abstract sylvesterDependencies: FactoryFunctionMap with get, set
        abstract schurDependencies: FactoryFunctionMap with get, set
        abstract lyapDependencies: FactoryFunctionMap with get, set
        abstract divideDependencies: FactoryFunctionMap with get, set
        abstract distanceDependencies: FactoryFunctionMap with get, set
        abstract intersectDependencies: FactoryFunctionMap with get, set
        abstract sumDependencies: FactoryFunctionMap with get, set
        abstract meanDependencies: FactoryFunctionMap with get, set
        abstract medianDependencies: FactoryFunctionMap with get, set
        abstract madDependencies: FactoryFunctionMap with get, set
        abstract varianceDependencies: FactoryFunctionMap with get, set
        abstract varDependencies: FactoryFunctionMap with get, set
        abstract quantileSeqDependencies: FactoryFunctionMap with get, set
        abstract stdDependencies: FactoryFunctionMap with get, set
        abstract combinationsDependencies: FactoryFunctionMap with get, set
        abstract gammaDependencies: FactoryFunctionMap with get, set
        abstract factorialDependencies: FactoryFunctionMap with get, set
        abstract kldivergenceDependencies: FactoryFunctionMap with get, set
        abstract multinomialDependencies: FactoryFunctionMap with get, set
        abstract permutationsDependencies: FactoryFunctionMap with get, set
        abstract pickRandomDependencies: FactoryFunctionMap with get, set
        abstract randomDependencies: FactoryFunctionMap with get, set
        abstract randomIntDependencies: FactoryFunctionMap with get, set
        abstract stirlingS2Dependencies: FactoryFunctionMap with get, set
        abstract bellNumbersDependencies: FactoryFunctionMap with get, set
        abstract catalanDependencies: FactoryFunctionMap with get, set
        abstract compositionDependencies: FactoryFunctionMap with get, set
        abstract simplifyDependencies: FactoryFunctionMap with get, set
        abstract derivativeDependencies: FactoryFunctionMap with get, set
        abstract rationalizeDependencies: FactoryFunctionMap with get, set
        abstract reviverDependencies: FactoryFunctionMap with get, set
        abstract eDependencies: FactoryFunctionMap with get, set
        abstract EDependencies: FactoryFunctionMap with get, set
        abstract falseDependencies: FactoryFunctionMap with get, set
        abstract iDependencies: FactoryFunctionMap with get, set
        abstract InfinityDependencies: FactoryFunctionMap with get, set
        abstract LN10Dependencies: FactoryFunctionMap with get, set
        abstract LN2Dependencies: FactoryFunctionMap with get, set
        abstract LOG10EDependencies: FactoryFunctionMap with get, set
        abstract LOG2EDependencies: FactoryFunctionMap with get, set
        abstract NaNDependencies: FactoryFunctionMap with get, set
        abstract nullDependencies: FactoryFunctionMap with get, set
        abstract phiDependencies: FactoryFunctionMap with get, set
        abstract piDependencies: FactoryFunctionMap with get, set
        abstract PIDependencies: FactoryFunctionMap with get, set
        abstract SQRT1_2Dependencies: FactoryFunctionMap with get, set
        abstract SQRT2Dependencies: FactoryFunctionMap with get, set
        abstract tauDependencies: FactoryFunctionMap with get, set
        abstract trueDependencies: FactoryFunctionMap with get, set
        abstract versionDependencies: FactoryFunctionMap with get, set
        abstract atomicMassDependencies: FactoryFunctionMap with get, set
        abstract avogadroDependencies: FactoryFunctionMap with get, set
        abstract bohrMagnetonDependencies: FactoryFunctionMap with get, set
        abstract bohrRadiusDependencies: FactoryFunctionMap with get, set
        abstract boltzmannDependencies: FactoryFunctionMap with get, set
        abstract classicalElectronRadiusDependencies: FactoryFunctionMap with get, set
        abstract conductanceQuantumDependencies: FactoryFunctionMap with get, set
        abstract coulombDependencies: FactoryFunctionMap with get, set
        abstract deuteronMassDependencies: FactoryFunctionMap with get, set
        abstract efimovFactorDependencies: FactoryFunctionMap with get, set
        abstract electricConstantDependencies: FactoryFunctionMap with get, set
        abstract electronMassDependencies: FactoryFunctionMap with get, set
        abstract elementaryChargeDependencies: FactoryFunctionMap with get, set
        abstract faradayDependencies: FactoryFunctionMap with get, set
        abstract fermiCouplingDependencies: FactoryFunctionMap with get, set
        abstract fineStructureDependencies: FactoryFunctionMap with get, set
        abstract firstRadiationDependencies: FactoryFunctionMap with get, set
        abstract gasConstantDependencies: FactoryFunctionMap with get, set
        abstract gravitationConstantDependencies: FactoryFunctionMap with get, set
        abstract gravityDependencies: FactoryFunctionMap with get, set
        abstract hartreeEnergyDependencies: FactoryFunctionMap with get, set
        abstract inverseConductanceQuantumDependencies: FactoryFunctionMap with get, set
        abstract klitzingDependencies: FactoryFunctionMap with get, set
        abstract loschmidtDependencies: FactoryFunctionMap with get, set
        abstract magneticConstantDependencies: FactoryFunctionMap with get, set
        abstract magneticFluxQuantumDependencies: FactoryFunctionMap with get, set
        abstract molarMassDependencies: FactoryFunctionMap with get, set
        abstract molarMassC12Dependencies: FactoryFunctionMap with get, set
        abstract molarPlanckConstantDependencies: FactoryFunctionMap with get, set
        abstract molarVolumeDependencies: FactoryFunctionMap with get, set
        abstract neutronMassDependencies: FactoryFunctionMap with get, set
        abstract nuclearMagnetonDependencies: FactoryFunctionMap with get, set
        abstract planckChargeDependencies: FactoryFunctionMap with get, set
        abstract planckConstantDependencies: FactoryFunctionMap with get, set
        abstract planckLengthDependencies: FactoryFunctionMap with get, set
        abstract planckMassDependencies: FactoryFunctionMap with get, set
        abstract planckTemperatureDependencies: FactoryFunctionMap with get, set
        abstract planckTimeDependencies: FactoryFunctionMap with get, set
        abstract protonMassDependencies: FactoryFunctionMap with get, set
        abstract quantumOfCirculationDependencies: FactoryFunctionMap with get, set
        abstract reducedPlanckConstantDependencies: FactoryFunctionMap with get, set
        abstract rydbergDependencies: FactoryFunctionMap with get, set
        abstract sackurTetrodeDependencies: FactoryFunctionMap with get, set
        abstract secondRadiationDependencies: FactoryFunctionMap with get, set
        abstract speedOfLightDependencies: FactoryFunctionMap with get, set
        abstract stefanBoltzmannDependencies: FactoryFunctionMap with get, set
        abstract thomsonCrossSectionDependencies: FactoryFunctionMap with get, set
        abstract vacuumImpedanceDependencies: FactoryFunctionMap with get, set
        abstract weakMixingAngleDependencies: FactoryFunctionMap with get, set
        abstract wienDisplacementDependencies: FactoryFunctionMap with get, set
        abstract applyTransformDependencies: FactoryFunctionMap with get, set
        abstract columnTransformDependencies: FactoryFunctionMap with get, set
        abstract filterTransformDependencies: FactoryFunctionMap with get, set
        abstract forEachTransformDependencies: FactoryFunctionMap with get, set
        abstract indexTransformDependencies: FactoryFunctionMap with get, set
        abstract mapTransformDependencies: FactoryFunctionMap with get, set
        abstract maxTransformDependencies: FactoryFunctionMap with get, set
        abstract meanTransformDependencies: FactoryFunctionMap with get, set
        abstract minTransformDependencies: FactoryFunctionMap with get, set
        abstract rangeTransformDependencies: FactoryFunctionMap with get, set
        abstract rowTransformDependencies: FactoryFunctionMap with get, set
        abstract subsetTransformDependencies: FactoryFunctionMap with get, set
        abstract concatTransformDependencies: FactoryFunctionMap with get, set
        abstract stdTransformDependencies: FactoryFunctionMap with get, set
        abstract sumTransformDependencies: FactoryFunctionMap with get, set
        abstract varianceTransformDependencies: FactoryFunctionMap with get, set

    type [<AllowNullLiteral>] Matrix =
        abstract ``type``: string with get, set
        abstract storage: unit -> string
        abstract datatype: unit -> string
        abstract create: data: MathArray * ?datatype: string -> unit
        abstract density: unit -> float
        abstract subset: index: Index * ?replacement: obj * ?defaultValue: obj -> Matrix
        abstract apply: dim: float * callback: (MathCollection -> float) -> MathCollection
        abstract get: index: ResizeArray<float> -> obj option
        abstract set: index: ResizeArray<float> * value: obj option * ?defaultValue: U2<float, string> -> Matrix
        abstract resize: size: MathCollection * ?defaultValue: U2<float, string> -> Matrix
        abstract clone: unit -> Matrix
        abstract size: unit -> ResizeArray<float>
        abstract map: callback: (obj option -> ResizeArray<float> -> Matrix -> obj option) * ?skipZeros: bool -> Matrix
        abstract forEach: callback: (obj option -> ResizeArray<float> -> Matrix -> unit) * ?skipZeros: bool -> unit
        abstract toArray: unit -> MathArray
        abstract valueOf: unit -> MathArray
        abstract format: ?options: U3<FormatOptions, float, (obj option -> string)> -> string
        abstract toString: unit -> string
        abstract toJSON: unit -> obj option
        abstract diagonal: ?k: U2<float, BigNumber> -> ResizeArray<obj option>
        abstract swapRows: i: float * j: float -> Matrix

    type [<AllowNullLiteral>] MatrixCtor =
        interface end

    type [<AllowNullLiteral>] MatrixCtorStatic =
        [<EmitConstructor>] abstract Create: unit -> MatrixCtor

    type [<AllowNullLiteral>] BigNumber =
        inherit Decimal

    type [<AllowNullLiteral>] Fraction =
        abstract s: float with get, set
        abstract n: float with get, set
        abstract d: float with get, set

    type [<AllowNullLiteral>] Complex =
        abstract re: float with get, set
        abstract im: float with get, set
        abstract clone: unit -> Complex
        abstract equals: other: Complex -> bool
        abstract format: ?precision: float -> string
        abstract fromJSON: json: obj -> Complex
        abstract fromPolar: polar: obj -> Complex
        abstract fromPolar: r: float * phi: float -> Complex
        abstract toJSON: unit -> obj
        abstract toPolar: unit -> PolarCoordinates
        abstract toString: unit -> string
        abstract compare: a: Complex * b: Complex -> float

    type [<AllowNullLiteral>] PolarCoordinates =
        abstract r: float with get, set
        abstract phi: float with get, set

    type [<AllowNullLiteral>] MathJSON =
        abstract mathjs: string option with get, set
        abstract value: float with get, set
        abstract unit: string with get, set
        abstract fixPrefix: bool option with get, set

    type [<AllowNullLiteral>] UnitComponent =
        abstract power: float with get, set
        abstract prefix: string with get, set
        abstract unit: UnitComponentUnit with get, set

    type [<AllowNullLiteral>] UnitPrefix =
        abstract name: string with get, set
        abstract value: float with get, set
        abstract scientific: bool with get, set

    type [<AllowNullLiteral>] Unit =
        abstract valueOf: unit -> string
        abstract clone: unit -> Unit
        abstract hasBase: ``base``: obj option -> bool
        abstract equalBase: unit: Unit -> bool
        abstract equals: unit: Unit -> bool
        abstract multiply: unit: Unit -> Unit
        abstract divide: unit: Unit -> Unit
        abstract pow: unit: Unit -> Unit
        abstract abs: unit: Unit -> Unit
        abstract ``to``: unit: string -> Unit
        abstract toNumber: ?unit: string -> float
        abstract toNumeric: ?unit: string -> U3<float, Fraction, BigNumber>
        abstract toSI: unit -> Unit
        abstract toString: unit -> string
        abstract toJSON: unit -> MathJSON
        abstract formatUnits: unit -> string
        abstract format: options: FormatOptions -> string
        abstract simplify: unit -> Unit
        abstract splitUnit: parts: ResizeArray<U2<string, Unit>> -> ResizeArray<Unit>
        abstract units: ResizeArray<UnitComponent> with get, set
        abstract dimensions: ResizeArray<float> with get, set
        abstract value: float with get, set
        abstract fixPrefix: bool with get, set
        abstract skipAutomaticSimplification: bool with get, set

    type [<AllowNullLiteral>] CreateUnitOptions =
        abstract prefixes: CreateUnitOptionsPrefixes option with get, set
        abstract aliases: ResizeArray<string> option with get, set
        abstract offset: float option with get, set
        abstract ``override``: bool option with get, set

    type SimplifyContext =
        obj

    type [<AllowNullLiteral>] SimplifyOptions =
        /// <summary>A boolean which is <c>true</c> by default.</summary>
        abstract exactFractions: bool option with get, set
        /// <summary>
        /// When <c>exactFractions</c> is true, a fraction will be returned only
        /// when both numerator and denominator are smaller than <c>fractionsLimit</c>.
        /// Default value is 10000.
        /// </summary>
        abstract fractionsLimit: float option with get, set
        /// <summary>A boolean which is <c>false</c> by default.</summary>
        abstract consoleDebug: bool option with get, set
        /// gives properties of each operator, which determine what simplifications
        /// are allowed. Properties are commutative, associative, total (whether
        /// the operation is defined for all arguments), and trivial (whether
        /// the operation applied to a single argument leaves that argument
        /// unchanged).
        abstract context: SimplifyContext option with get, set

    type SimplifyRule =
        U4<SimplifyRuleCase1, {| s: string; repeat: bool option; assuming: SimplifyContext option; imposeContext: SimplifyContext option |}, string, (MathNode -> MathNode)>

    type [<AllowNullLiteral>] SimplifyRuleCase1 =
        abstract l: string with get, set
        abstract r: string with get, set
        abstract repeat: bool option with get, set
        abstract assuming: SimplifyContext option with get, set
        abstract imposeContext: SimplifyContext option with get, set

    type [<AllowNullLiteral>] Simplify =
        [<Emit("$0($1...)")>] abstract Invoke: expr: U2<MathNode, string> -> MathNode
        [<Emit("$0($1...)")>] abstract Invoke: expr: U2<MathNode, string> * rules: ResizeArray<SimplifyRule> * ?scope: obj * ?options: SimplifyOptions -> MathNode
        [<Emit("$0($1...)")>] abstract Invoke: expr: U2<MathNode, string> * scope: obj * ?options: SimplifyOptions -> MathNode
        abstract rules: ResizeArray<SimplifyRule> with get, set

    type [<AllowNullLiteral>] UnitDefinition =
        abstract definition: U2<string, Unit> option with get, set
        abstract prefixes: string option with get, set
        abstract offset: float option with get, set
        abstract aliases: ResizeArray<string> option with get, set
        abstract baseName: string option with get, set

    type [<AllowNullLiteral>] Index =
        interface end

    type [<AllowNullLiteral>] EvalFunction =
        abstract evaluate: ?scope: obj -> obj option

    type [<AllowNullLiteral>] MathNode =
        abstract isNode: bool with get, set
        abstract comment: string with get, set
        abstract ``type``: string with get, set
        abstract isUpdateNode: bool option with get, set
        /// Create a shallow clone of the node. The node itself is cloned, its
        /// childs are not cloned.
        abstract clone: unit -> MathNode
        /// Create a deep clone of the node. Both the node as well as all its
        /// childs are cloned recursively.
        abstract cloneDeep: unit -> MathNode
        /// Compile an expression into optimized JavaScript code. compile returns
        /// an object with a function evaluate([scope]) to evaluate. Example:
        abstract compile: unit -> EvalFunction
        /// Compile and eval an expression, this is the equivalent of doing
        /// node.compile().evaluate(scope). Example:
        abstract evaluate: ?expr: obj -> obj option
        /// Test whether this node equals an other node. Does a deep comparison
        /// of the values of both nodes.
        abstract equals: other: MathNode -> bool
        /// <summary>
        /// 
        /// Filter nodes in an expression tree. The callback function is called
        /// as callback(node: MathNode, path: string, parent: MathNode) : boolean
        /// for every node in the tree, and must return a boolean. The function
        /// filter returns an array with nodes for which the test returned true.
        /// Parameter path is a string containing a relative JSON Path.
        /// 
        /// Example:
        /// 
        /// <code>
        /// var node = math.parse('x^2 + x/4 + 3*y');
        /// var filtered = node.filter(function (node) {
        /// return node.isSymbolMathNode &amp;&amp; node.name == 'x';
        /// });
        /// // returns an array with two entries: two SymbolMathNodes 'x'
        /// </code>
        /// 
        /// The callback function is called as callback(node: MathNode, path:
        /// string, parent: MathNode) : boolean for every node in the tree, and
        /// must return a boolean. The function filter returns an array with
        /// nodes for which the test returned true. Parameter path is a string
        /// containing a relative JSON Path.
        /// </summary>
        /// <returns>Returns an array with nodes for which test returned true</returns>
        abstract filter: callback: (MathNode -> string -> MathNode -> obj option) -> ResizeArray<MathNode>
        /// [forEach description]
        abstract forEach: callback: (MathNode -> string -> MathNode -> unit) -> unit
        /// <summary>
        /// Transform a node. Creates a new MathNode having it’s child's be the
        /// results of calling the provided callback function for each of the
        /// child's of the original node. The callback function is called as
        /// <c>callback(child: MathNode, path: string, parent: MathNode)</c> and must
        /// return a MathNode. Parameter path is a string containing a relative
        /// JSON Path.
        /// 
        /// 
        /// See also transform, which is a recursive version of map.
        /// </summary>
        abstract map: callback: (MathNode -> string -> MathNode -> MathNode) -> MathNode
        /// Get a HTML representation of the parsed expression.
        abstract toHTML: ?options: obj -> string
        /// Get a string representation of the parsed expression. This is not
        /// exactly the same as the original input.
        abstract toString: ?options: obj -> string
        /// Get a LaTeX representation of the expression.
        abstract toTex: ?options: obj -> string
        /// Recursively transform an expression tree via a transform function.
        /// Similar to Array.map, but recursively executed on all nodes in the
        /// expression tree. The callback function is a mapping function
        /// accepting a node, and returning a replacement for the node or the
        /// original node. Function callback is called as callback(node:
        /// MathNode, path: string, parent: MathNode) for every node in the tree,
        /// and must return a MathNode. Parameter path is a string containing a
        /// relative JSON Path.
        /// 
        /// For example, to replace all nodes of type SymbolMathNode having name
        /// ‘x’ with a ConstantMathNode with value 3:
        /// <code lang="js">
        /// var node = math.parse('x^2 + 5*x');
        /// var transformed = node.transform(function (node, path, parent) {
        ///   if (node.SymbolMathNode && node.name == 'x') {
        ///     return new math.expression.node.ConstantMathNode(3);
        ///   }
        ///   else {
        ///     return node;
        ///   }
        /// });
        /// transformed.toString(); // returns '(3 ^ 2) + (5 * 3)'
        /// </code>
        abstract transform: callback: (MathNode -> string -> MathNode -> 'TResult) -> 'TResult
        /// <summary>
        /// <c>traverse(callback)</c>
        /// 
        /// Recursively traverse all nodes in a node tree. Executes given
        /// callback for this node and each of its child nodes. Similar to
        /// Array.forEach, except recursive. The callback function is a mapping
        /// function accepting a node, and returning a replacement for the node
        /// or the original node. Function callback is called as callback(node:
        /// MathNode, path: string, parent: MathNode) for every node in the tree.
        /// Parameter path is a string containing a relative JSON Path. Example:
        /// 
        /// <code>
        /// var node = math.parse('3 * x + 2');
        /// node.traverse(function (node, path, parent) {
        /// switch (node.type) {
        /// case 'OperatorMathNode': console.log(node.type, node.op);    break;
        /// case 'ConstantMathNode': console.log(node.type, node.value); break;
        /// case 'SymbolMathNode':   console.log(node.type, node.name);  break;
        /// default:             console.log(node.type);
        /// }
        /// });
        /// // outputs:
        /// //   OperatorMathNode +
        /// //   OperatorMathNode *
        /// //   ConstantMathNode 3
        /// //   SymbolMathNode x
        /// //   ConstantMathNode 2
        /// </code>
        /// </summary>
        abstract traverse: callback: (MathNode -> string -> MathNode -> unit) -> unit

    type [<AllowNullLiteral>] Parser =
        abstract evaluate: expr: U2<string, ResizeArray<string>> -> obj option
        abstract get: variable: string -> obj option
        abstract getAll: unit -> ParserGetAllReturn
        abstract set: (string -> obj option -> unit) with get, set
        abstract clear: (unit -> unit) with get, set

    type [<AllowNullLiteral>] ParserGetAllReturn =
        [<EmitIndexer>] abstract Item: key: string -> obj option with get, set

    type [<AllowNullLiteral>] Distribution =
        abstract random: size: obj option * ?min: obj * ?max: obj -> obj option
        abstract randomInt: min: obj option * ?max: obj -> obj option
        abstract pickRandom: array: obj option -> obj option

    type [<AllowNullLiteral>] FormatOptions =
        /// Number notation. Choose from: 'fixed' Always use regular number
        /// notation. For example '123.40' and '14000000' 'exponential' Always
        /// use exponential notation. For example '1.234e+2' and '1.4e+7' 'auto'
        /// (default) Regular number notation for numbers having an absolute
        /// value between lower and upper bounds, and uses exponential notation
        /// elsewhere. Lower bound is included, upper bound is excluded. For
        /// example '123.4' and '1.4e7'.
        abstract notation: FormatOptionsNotation option with get, set
        /// A number between 0 and 16 to round the digits of the number. In case
        /// of notations 'exponential' and 'auto', precision defines the total
        /// number of significant digits returned and is undefined by default. In
        /// case of notation 'fixed', precision defines the number of significant
        /// digits after the decimal point, and is 0 by default.
        abstract precision: float option with get, set
        /// Exponent determining the lower boundary for formatting a value with
        /// an exponent when notation='auto. Default value is -3.
        abstract lowerExp: float option with get, set
        /// Exponent determining the upper boundary for formatting a value with
        /// an exponent when notation='auto. Default value is 5.
        abstract upperExp: float option with get, set
        /// Available values: 'ratio' (default) or 'decimal'. For example
        /// format(fraction(1, 3)) will output '1/3' when 'ratio' is configured,
        /// and will output 0.(3) when 'decimal' is configured.
        abstract fraction: string option with get, set

    type [<AllowNullLiteral>] Help =
        abstract toString: unit -> string
        abstract toJSON: unit -> string

    type [<AllowNullLiteral>] ConfigOptions =
        abstract epsilon: float option with get, set
        abstract matrix: ConfigOptionsMatrix option with get, set
        abstract number: ConfigOptionsNumber option with get, set
        abstract precision: float option with get, set
        abstract predictable: bool option with get, set
        abstract randomSeed: string option with get, set

    type [<AllowNullLiteral>] MathJsChain<'TValue> =
        abstract ``done``: unit -> 'TValue
        /// Create a BigNumber, which can store numbers with arbitrary precision.
        /// When a matrix is provided, all elements will be converted to
        /// BigNumber.
        abstract bignumber: this: MathJsChain<U5<float, string, Fraction, BigNumber, bool> option> -> MathJsChain<BigNumber>
        abstract bignumber: this: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// Create a boolean or convert a string or number to a boolean. In case
        /// of a number, true is returned for non-zero numbers, and false in case
        /// of zero. Strings can be 'true' or 'false', or can contain a number.
        /// When value is a matrix, all elements will be converted to boolean.
        abstract boolean: this: MathJsChain<U3<string, float, bool> option> -> MathJsChain<bool>
        abstract boolean: this: MathJsChain<MathCollection> -> MathJsChain<MathCollection>
        /// <summary>Create a complex value or convert a value to a complex value.</summary>
        /// <param name="im">
        /// Argument specifying the imaginary part of the complex
        /// number
        /// </param>
        abstract complex: this: MathJsChain<U3<Complex, string, PolarCoordinates>> * ?im: float -> MathJsChain<Complex>
        abstract complex: this: MathJsChain<MathCollection> -> MathJsChain<MathCollection>
        /// <summary>Create a user-defined unit and register it with the Unit type.</summary>
        /// <param name="definition">
        /// Definition of the unit in terms of existing units.
        /// For example, ‘0.514444444 m / s’.
        /// </param>
        /// <param name="options">
        /// (optional) An object containing any of the following
        /// properties:&lt;/br&gt;- prefixes {string} “none”, “short”, “long”,
        /// “binary_short”, or “binary_long”. The default is “none”.&lt;/br&gt;-
        /// aliases {Array} Array of strings. Example: [‘knots’, ‘kt’,
        /// ‘kts’]&lt;/br&gt;- offset {Numeric} An offset to apply when converting from
        /// the unit. For example, the offset for celsius is 273.15. Default is
        /// 0.
        /// </param>
        abstract createUnit: this: MathJsChain<string> * ?definition: U3<string, UnitDefinition, Unit> * ?options: CreateUnitOptions -> MathJsChain<Unit>
        /// <summary>Create a user-defined unit and register it with the Unit type.</summary>
        /// <param name="options">
        /// (optional) An object containing any of the following
        /// properties:&lt;/br&gt;- prefixes {string} “none”, “short”, “long”,
        /// “binary_short”, or “binary_long”. The default is “none”.&lt;/br&gt;-
        /// aliases {Array} Array of strings. Example: [‘knots’, ‘kt’,
        /// ‘kts’]&lt;/br&gt;- offset {Numeric} An offset to apply when converting from
        /// the unit. For example, the offset for celsius is 273.15. Default is
        /// 0.
        /// </param>
        abstract createUnit: this: MathJsChain<Record<string, U3<string, UnitDefinition, Unit>>> * ?options: CreateUnitOptions -> MathJsChain<Unit>
        /// <summary>Create a fraction convert a value to a fraction.</summary>
        /// <param name="denominator">
        /// Argument specifying the denominator of the
        /// fraction
        /// </param>
        abstract fraction: this: MathJsChain<U5<float, string, BigNumber, Fraction, FractionDefinition>> * ?denominator: float -> MathJsChain<Fraction>
        abstract fraction: this: MathJsChain<MathCollection> -> MathJsChain<MathCollection>
        /// Create an index. An Index can store ranges having start, step, and
        /// end for multiple dimensions. Matrix.get, Matrix.set, and math.subset
        /// accept an Index as input.
        abstract index: this: MathJsChain<ResizeArray<obj option>> -> MathJsChain<Index>
        /// Create a Matrix. The function creates a new math.type.Matrix object
        /// from an Array. A Matrix has utility functions to manipulate the data
        /// in the matrix, like getting the size and getting or setting values in
        /// the matrix. Supported storage formats are 'dense' and 'sparse'.
        abstract matrix: this: MathJsChain<MathCollection> * ?format: MathJsStaticMatrix * ?dataType: string -> MathJsChain<Matrix>
        /// <summary>
        /// Create a number or convert a string, boolean, or unit to a number.
        /// When value is a matrix, all elements will be converted to number.
        /// </summary>
        /// <param name="valuelessUnit">
        /// A valueless unit, used to convert a unit to a
        /// number
        /// </param>
        abstract number: this: MathJsChain<U6<string, float, BigNumber, Fraction, bool, Unit> option> * ?valuelessUnit: U2<Unit, string> -> MathJsChain<float>
        abstract number: this: MathJsChain<MathCollection> * ?valuelessUnit: U2<Unit, string> -> MathJsChain<MathCollection>
        /// <summary>
        /// Create a Sparse Matrix. The function creates a new math.type.Matrix
        /// object from an Array. A Matrix has utility functions to manipulate
        /// the data in the matrix, like getting the size and getting or setting
        /// values in the matrix.
        /// </summary>
        /// <param name="dataType">Sparse Matrix data type</param>
        abstract sparse: this: MathJsChain<MathCollection> * ?dataType: string -> MathJsChain<Matrix>
        /// <summary>
        /// Split a unit in an array of units whose sum is equal to the original
        /// unit.
        /// </summary>
        /// <param name="parts">An array of strings or valueless units</param>
        abstract splitUnit: this: MathJsChain<Unit> * parts: ResizeArray<Unit> -> MathJsChain<ResizeArray<Unit>>
        /// Create a string or convert any object into a string. Elements of
        /// Arrays and Matrices are processed element wise.
        abstract string: this: MathJsChain<U3<MathNumericType, string, Unit> option> -> MathJsChain<string>
        abstract string: this: MathJsChain<MathCollection> -> MathJsChain<MathCollection>
        /// <summary>
        /// Create a unit. Depending on the passed arguments, the function will
        /// create and return a new math.type.Unit object. When a matrix is
        /// provided, all elements will be converted to units.
        /// </summary>
        /// <param name="unit">The unit to be created</param>
        abstract unit: this: MathJsChain<string> * ?unit: string -> MathJsChain<Unit>
        abstract unit: this: MathJsChain<MathNumericType> * ?unit: string -> MathJsChain<Unit>
        abstract unit: this: MathJsChain<MathCollection> * ?unit: string -> MathJsChain<ResizeArray<Unit>>
        /// Parse and compile an expression. Returns a an object with a function
        /// evaluate([scope]) to evaluate the compiled expression.
        abstract compile: this: MathJsChain<MathExpression> -> MathJsChain<EvalFunction>
        /// <summary>Evaluate an expression.</summary>
        /// <param name="scope">Scope to read/write variables</param>
        abstract evaluate: this: MathJsChain<U2<MathExpression, Matrix>> * ?scope: obj -> MathJsChain<obj option>
        abstract evaluate: this: MathJsChain<ResizeArray<MathExpression>> * ?scope: obj -> MathJsChain<ResizeArray<obj option>>
        /// Retrieve help on a function or data type. Help files are retrieved
        /// from the documentation in math.expression.docs.
        abstract help: this: MathJsChain<obj> -> MathJsChain<obj>
        /// <param name="options">Available options: nodes - a set of custome nodes</param>
        abstract parse: this: MathJsChain<ResizeArray<MathExpression>> * ?options: obj -> MathJsChain<ResizeArray<MathNode>>
        /// <summary>
        /// Parse an expression. Returns a node tree, which can be evaluated by
        /// invoking node.evaluate();
        /// </summary>
        /// <param name="options">Available options: nodes - a set of custome nodes</param>
        abstract parse: this: MathJsChain<MathExpression> * ?options: obj -> MathJsChain<MathNode>
        /// <summary>Replaces variable nodes with their scoped values</summary>
        /// <param name="scope">Scope to read/write variables</param>
        abstract resolve: this: MathJsChain<MathNode> * ?scope: Record<string, obj option> -> MathJsChain<MathNode>
        abstract resolve: this: MathJsChain<ResizeArray<MathNode>> * ?scope: Record<string, obj option> -> MathJsChain<ResizeArray<MathNode>>
        /// <param name="variable">The variable over which to differentiate</param>
        /// <param name="options">
        /// There is one option available, simplify, which is true
        /// by default. When false, output will not be simplified.
        /// </param>
        abstract derivative: this: MathJsChain<U2<MathNode, string>> * variable: U2<MathNode, string> * ?options: {| simplify: bool |} -> MathJsChain<MathNode>
        /// <summary>
        /// Solves the linear equation system by forwards substitution. Matrix
        /// must be a lower triangular matrix.
        /// </summary>
        /// <param name="b">A column vector with the b values</param>
        abstract lsolve: this: MathJsChain<Matrix> * b: MathCollection -> MathJsChain<Matrix>
        abstract lsolve: this: MathJsChain<MathArray> * b: MathCollection -> MathJsChain<MathArray>
        /// Calculate the Matrix LU decomposition with partial pivoting. Matrix A
        /// is decomposed in two matrices (L, U) and a row permutation vector p
        /// where A[p,:] = L * U
        abstract lup: this: MathJsChain<MathCollection> -> MathJsChain<LUDecomposition>
        /// <summary>
        /// Solves the linear system A * x = b where A is an [n x n] matrix and b
        /// is a [n] column vector.
        /// </summary>
        /// <param name="b">Column Vector</param>
        /// <param name="order">
        /// The Symbolic Ordering and Analysis order, see slu for
        /// details. Matrix must be a SparseMatrix
        /// </param>
        /// <param name="threshold">
        /// Partial pivoting threshold (1 for partial pivoting),
        /// see slu for details. Matrix must be a SparseMatrix.
        /// </param>
        abstract lusolve: this: MathJsChain<Matrix> * b: MathCollection * ?order: float * ?threshold: float -> MathJsChain<Matrix>
        abstract lusolve: this: MathJsChain<MathArray> * b: MathCollection * ?order: float * ?threshold: float -> MathJsChain<MathArray>
        abstract lusolve: this: MathJsChain<LUDecomposition> * b: MathCollection -> MathJsChain<Matrix>
        /// Calculate the Matrix QR decomposition. Matrix A is decomposed in two
        /// matrices (Q, R) where Q is an orthogonal matrix and R is an upper
        /// triangular matrix.
        abstract qr: this: MathJsChain<MathCollection> -> MathJsChain<QRDecomposition>
        /// <summary>
        /// Transform a rationalizable expression in a rational fraction. If
        /// rational fraction is one variable polynomial then converts the
        /// numerator and denominator in canonical form, with decreasing
        /// exponents, returning the coefficients of numerator.
        /// </summary>
        /// <param name="optional">
        /// scope of expression or true for already evaluated
        /// rational expression at input
        /// </param>
        /// <param name="detailed">
        /// optional True if return an object, false if return
        /// expression node (default)
        /// </param>
        abstract rationalize: this: MathJsChain<U2<MathNode, string>> * ?optional: U2<obj, bool> * ?detailed: bool -> MathJsChain<MathNode>
        /// <summary>Simplify an expression tree.</summary>
        /// <param name="rules">
        /// A list of rules are applied to an expression, repeating
        /// over the list until no further changes are made. It’s possible to
        /// pass a custom set of rules to the function as second argument. A rule
        /// can be specified as an object, string, or function.
        /// </param>
        /// <param name="scope">Scope to variables</param>
        /// <param name="options">Options to configure the behavior of simplify</param>
        abstract simplify: this: MathJsChain<U2<MathNode, string>> * ?rules: ResizeArray<SimplifyRule> * ?scope: U2<Map<string, MathType>, obj> * ?options: SimplifyOptions -> MathJsChain<MathNode>
        abstract simplifyConstant: this: MathJsChain<U2<MathNode, string>> * ?options: SimplifyOptions -> MathJsChain<MathNode>
        abstract simplifyCore: this: MathJsChain<U2<MathNode, string>> * ?options: SimplifyOptions -> MathJsChain<MathNode>
        /// <summary>
        /// Calculate the Sparse Matrix LU decomposition with full pivoting.
        /// Sparse Matrix A is decomposed in two matrices (L, U) and two
        /// permutation vectors (pinv, q) where P * A * Q = L * U
        /// </summary>
        /// <param name="order">
        /// The Symbolic Ordering and Analysis order: 0 - Natural
        /// ordering, no permutation vector q is returned 1 - Matrix must be
        /// square, symbolic ordering and analisis is performed on M = A + A' 2 -
        /// Symbolic ordering and analysis is performed on M = A' * A. Dense
        /// columns from A' are dropped, A recreated from A'. This is appropriate
        /// for LU factorization of non-symmetric matrices. 3 - Symbolic ordering
        /// and analysis is performed on M = A' * A. This is best used for LU
        /// factorization is matrix M has no dense rows. A dense row is a row
        /// with more than 10*sqr(columns) entries.
        /// </param>
        /// <param name="threshold">Partial pivoting threshold (1 for partial pivoting)</param>
        abstract slu: this: MathJsChain<Matrix> * order: float * threshold: float -> MathJsChain<SLUDecomposition>
        /// <summary>
        /// Solves the linear equation system by backward substitution. Matrix
        /// must be an upper triangular matrix. U * x = b
        /// </summary>
        /// <param name="b">A column vector with the b values</param>
        abstract usolve: this: MathJsChain<Matrix> * b: MathCollection -> MathJsChain<Matrix>
        abstract usolve: this: MathJsChain<MathArray> * b: MathCollection -> MathJsChain<MathArray>
        /// Calculate the absolute value of a number. For matrices, the function
        /// is evaluated element wise.
        abstract abs: this: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathType
        /// <summary>
        /// Add two values, x + y. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="y">Second value to add</param>
        abstract add: this: MathJsChain<'T> * y: 'T -> MathJsChain<'T> when 'T :> MathType
        abstract add: this: MathJsChain<MathType> * y: MathType -> MathJsChain<MathType>
        /// <summary>
        /// Apply a function that maps an array to a scalar along a given axis of the
        /// matrix or array. Returns a new matrix or array with one less dimension
        /// than the input.
        /// </summary>
        /// <param name="dim">The dimension along which the callback is applied</param>
        /// <param name="callback">
        /// The callback function that is applied. This Function should take an
        /// array or 1-d matrix as an input and return a number.
        /// </param>
        /// <returns>The residual matrix with the function applied over some dimension.</returns>
        abstract apply: this: MathJsChain<'T> * dim: float * callback: (U2<Array<MathType>, Matrix> -> float) -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Calculate the cubic root of a value. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="allRoots">
        /// Optional, false by default. Only applicable when x is
        /// a number or complex number. If true, all complex roots are returned,
        /// if false (default) the principal root is returned.
        /// </param>
        abstract cbrt: this: MathJsChain<'T> * ?allRoots: bool -> MathJsChain<'T>
        /// <summary>
        /// Round a value towards plus infinity If x is complex, both real and
        /// imaginary part are rounded towards plus infinity. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="n">Number of decimals Default value: 0.</param>
        abstract ceil: this: MathJsChain<'T> * ?n: U3<float, BigNumber, MathCollection> -> MathJsChain<'T>
        /// <summary>
        /// Round a value towards zero. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="n">Number of decimals Default value: 0.</param>
        abstract fix: this: MathJsChain<'T> * ?n: U3<float, BigNumber, MathCollection> -> MathJsChain<'T>
        /// <summary>
        /// Round a value towards minus infinity. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="n">Number of decimals Default value: 0.</param>
        abstract floor: this: MathJsChain<'T> * ?n: U3<float, BigNumber, MathCollection> -> MathJsChain<'T>
        /// <summary>
        /// Round a value towards the nearest integer. For matrices, the function
        /// is evaluated element wise.
        /// </summary>
        /// <param name="n">Number of decimals Default value: 0.</param>
        abstract round: this: MathJsChain<'T> * ?n: U3<float, BigNumber, MathCollection> -> MathJsChain<'T>
        /// Compute the cube of a value, x * x * x. For matrices, the function is
        /// evaluated element wise.
        abstract cube: this: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Divide two values, x / y. To divide matrices, x is multiplied with
        /// the inverse of y: x * inv(y).
        /// </summary>
        /// <param name="y">Denominator</param>
        abstract divide: this: MathJsChain<Unit> * y: Unit -> MathJsChain<U2<Unit, float>>
        abstract divide: this: MathJsChain<Unit> * y: float -> MathJsChain<Unit>
        abstract divide: this: MathJsChain<float> * y: float -> MathJsChain<float>
        abstract divide: this: MathJsChain<MathType> * y: MathType -> MathJsChain<MathType>
        /// <summary>
        /// Divide two matrices element wise. The function accepts both matrices
        /// and scalar values.
        /// </summary>
        /// <param name="y">Denominator</param>
        abstract dotDivide: this: MathJsChain<'T> * y: MathType -> MathJsChain<'T> when 'T :> MathCollection
        abstract dotDivide: this: MathJsChain<MathType> * y: 'T -> MathJsChain<'T> when 'T :> MathCollection
        abstract dotDivide: this: MathJsChain<Unit> * y: MathType -> MathJsChain<Unit>
        abstract dotDivide: this: MathJsChain<MathType> * y: Unit -> MathJsChain<Unit>
        abstract dotDivide: this: MathJsChain<MathNumericType> * y: MathNumericType -> MathJsChain<MathNumericType>
        /// <summary>
        /// Multiply two matrices element wise. The function accepts both
        /// matrices and scalar values.
        /// </summary>
        /// <param name="y">Right hand value</param>
        abstract dotMultiply: this: MathJsChain<'T> * y: MathType -> MathJsChain<'T> when 'T :> MathCollection
        abstract dotMultiply: this: MathJsChain<MathType> * y: 'T -> MathJsChain<'T> when 'T :> MathCollection
        abstract dotMultiply: this: MathJsChain<Unit> * y: MathType -> MathJsChain<Unit>
        abstract dotMultiply: this: MathJsChain<MathType> * y: Unit -> MathJsChain<Unit>
        abstract dotMultiply: this: MathJsChain<MathNumericType> * y: MathNumericType -> MathJsChain<MathNumericType>
        /// <summary>Calculates the power of x to y element wise.</summary>
        /// <param name="y">The exponent</param>
        abstract dotPow: this: MathJsChain<'T> * y: MathType -> MathJsChain<'T> when 'T :> MathType
        /// Calculate the exponent of a value. For matrices, the function is
        /// evaluated element wise.
        abstract exp: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the value of subtracting 1 from the exponential value. For
        /// matrices, the function is evaluated element wise.
        abstract expm1: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the greatest common divisor for two or more values or
        /// arrays. For matrices, the function is evaluated element wise.
        abstract gcd: this: MathJsChain<ResizeArray<'T>> * [<ParamArray>] args: 'T[] -> MathJsChain<'T>
        /// Calculate the hypotenusa of a list with values. The hypotenusa is
        /// defined as: hypot(a, b, c, ...) = sqrt(a^2 + b^2 + c^2 + ...) For
        /// matrix input, the hypotenusa is calculated for all values in the
        /// matrix.
        abstract hypot: this: MathJsChain<ResizeArray<'T>> -> MathJsChain<'T>
        /// <summary>
        /// Calculate the least common multiple for two or more values or arrays.
        /// lcm is defined as: lcm(a, b) = abs(a * b) / gcd(a, b) For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="b">An integer number</param>
        abstract lcm: this: MathJsChain<'T> * b: 'T -> MathJsChain<'T>
        /// <summary>
        /// Calculate the logarithm of a value. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="base">
        /// Optional base for the logarithm. If not provided, the
        /// natural logarithm of x is calculated. Default value: e.
        /// </param>
        abstract log: this: MathJsChain<'T> * ?``base``: U3<float, BigNumber, Complex> -> MathJsChain<NoLiteralType<'T>>
        /// Calculate the 10-base of a value. This is the same as calculating
        /// log(x, 10). For matrices, the function is evaluated element wise.
        abstract log10: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the logarithm of a value+1. For matrices, the function is
        /// evaluated element wise.
        abstract log1p: this: MathJsChain<float> * ?``base``: U3<float, BigNumber, Complex> -> MathJsChain<float>
        abstract log1p: this: MathJsChain<BigNumber> * ?``base``: U3<float, BigNumber, Complex> -> MathJsChain<BigNumber>
        abstract log1p: this: MathJsChain<Complex> * ?``base``: U3<float, BigNumber, Complex> -> MathJsChain<Complex>
        abstract log1p: this: MathJsChain<MathArray> * ?``base``: U3<float, BigNumber, Complex> -> MathJsChain<MathArray>
        abstract log1p: this: MathJsChain<Matrix> * ?``base``: U3<float, BigNumber, Complex> -> MathJsChain<Matrix>
        /// Calculate the 2-base of a value. This is the same as calculating
        /// log(x, 2). For matrices, the function is evaluated element wise.
        abstract log2: this: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Calculates the modulus, the remainder of an integer division. For
        /// matrices, the function is evaluated element wise. The modulus is
        /// defined as: x - y * floor(x / y)
        /// </summary>
        /// <seealso href="http://en.wikipedia.org/wiki/Modulo_operation." />
        /// <param name="y">Divisor</param>
        abstract ``mod``: this: MathJsChain<'T> * y: U4<float, BigNumber, Fraction, MathCollection> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Multiply two values, x * y. The result is squeezed. For matrices, the
        /// matrix product is calculated.
        /// </summary>
        /// <param name="y">The second value to multiply</param>
        abstract multiply: this: MathJsChain<'T> * y: MathType -> MathJsChain<'T> when 'T :> MathCollection
        abstract multiply: this: MathJsChain<Unit> * y: Unit -> MathJsChain<Unit>
        abstract multiply: this: MathJsChain<float> * y: float -> MathJsChain<float>
        abstract multiply: this: MathJsChain<MathType> * y: MathType -> MathJsChain<MathType>
        /// <summary>
        /// Calculate the norm of a number, vector or matrix. The second
        /// parameter p is optional. If not provided, it defaults to 2.
        /// </summary>
        /// <param name="p">
        /// Vector space. Supported numbers include Infinity and
        /// -Infinity. Supported strings are: 'inf', '-inf', and 'fro' (The
        /// Frobenius norm) Default value: 2.
        /// </param>
        abstract norm: this: MathJsChain<U4<float, BigNumber, Complex, MathCollection>> * ?p: U3<float, BigNumber, string> -> MathJsChain<U2<float, BigNumber>>
        /// <summary>
        /// Calculate the nth root of a value. The principal nth root of a
        /// positive real number A, is the positive real solution of the equation
        /// x^root = A For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="root">The root. Default value: 2.</param>
        abstract nthRoot: this: MathJsChain<U4<float, BigNumber, MathCollection, Complex>> * ?root: U2<float, BigNumber> -> MathJsChain<U3<float, Complex, MathCollection>>
        /// <summary>
        /// Calculates the power of x to y, x ^ y. Matrix exponentiation is
        /// supported for square matrices x, and positive integer exponents y.
        /// </summary>
        /// <param name="y">The exponent</param>
        abstract pow: this: MathJsChain<MathType> * y: U3<float, BigNumber, Complex> -> MathJsChain<MathType>
        /// <summary>
        /// Compute the sign of a value. The sign of a value x is: 1 when x &gt; 1
        /// -1 when x &lt; 0 0 when x == 0 For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="x">The number for which to determine the sign</param>
        /// <returns>The sign of x</returns>
        abstract sign: this: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathType
        /// Calculate the square root of a value. For matrices, the function is
        /// evaluated element wise.
        abstract sqrt: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Compute the square of a value, x * x. For matrices, the function is
        /// evaluated element wise.
        abstract square: this: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathType
        /// <summary>
        /// Subtract two values, x - y. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="y">Value to subtract from x</param>
        abstract subtract: this: MathJsChain<'T> * y: 'T -> MathJsChain<'T> when 'T :> MathType
        /// Inverse the sign of a value, apply a unary minus operation. For
        /// matrices, the function is evaluated element wise. Boolean values and
        /// strings will be converted to a number. For complex numbers, both real
        /// and complex value are inverted.
        abstract unaryMinus: this: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathType
        /// Unary plus operation. Boolean values and strings will be converted to
        /// a number, numeric values will be returned as is. For matrices, the
        /// function is evaluated element wise.
        abstract unaryPlus: this: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Calculate the extended greatest common divisor for two values. See
        /// <see href="http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm." />
        /// </summary>
        /// <param name="b">An integer number</param>
        abstract xgcd: this: MathJsChain<U2<float, BigNumber>> * b: U2<float, BigNumber> -> MathJsChain<MathArray>
        /// Count the number of elements of a matrix, array or string.
        abstract count: this: MathJsChain<MathCollection> -> MathJsChain<float>
        abstract count: this: MathJsChain<string> -> MathJsChain<float>
        /// Compute the sum of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the sum of all elements will be
        /// calculated.
        abstract sum: this: MathJsChain<Array<U3<float, BigNumber, Fraction>>> -> MathJsChain<float>
        abstract sum: this: MathJsChain<MathCollection> -> MathJsChain<float>
        /// <summary>
        /// Bitwise AND two values, x &amp; y. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to and</param>
        abstract bitAnd: this: MathJsChain<'T> * y: U3<float, BigNumber, MathCollection> -> MathJsChain<NoLiteralType<'T>>
        /// Bitwise NOT value, ~x. For matrices, the function is evaluated
        /// element wise. For units, the function is evaluated on the best prefix
        /// base.
        abstract bitNot: this: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Bitwise OR two values, x | y. For matrices, the function is evaluated
        /// element wise. For units, the function is evaluated on the lowest
        /// print base.
        /// </summary>
        /// <param name="y">Second value to or</param>
        abstract bitOr: this: MathJsChain<'T> * y: 'T -> MathJsChain<'T>
        /// <summary>
        /// Bitwise XOR two values, x ^ y. For matrices, the function is
        /// evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to xor</param>
        abstract bitXor: this: MathJsChain<'T> * y: U3<float, BigNumber, MathCollection> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Bitwise left logical shift of a value x by y number of bits, x &lt;&lt; y.
        /// For matrices, the function is evaluated element wise. For units, the
        /// function is evaluated on the best prefix base.
        /// </summary>
        /// <param name="y">Amount of shifts</param>
        abstract leftShift: this: MathJsChain<'T> * y: U2<float, BigNumber> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Bitwise right arithmetic shift of a value x by y number of bits, x &gt;&gt;
        /// y. For matrices, the function is evaluated element wise. For units,
        /// the function is evaluated on the best prefix base.
        /// </summary>
        /// <param name="y">Amount of shifts</param>
        abstract rightArithShift: this: MathJsChain<'T> * y: U2<float, BigNumber> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Bitwise right logical shift of value x by y number of bits, x &gt;&gt;&gt; y.
        /// For matrices, the function is evaluated element wise. For units, the
        /// function is evaluated on the best prefix base.
        /// </summary>
        /// <param name="y">Amount of shifts</param>
        abstract rightLogShift: this: MathJsChain<'T> * y: float -> MathJsChain<NoLiteralType<'T>>
        /// The Bell Numbers count the number of partitions of a set. A partition
        /// is a pairwise disjoint subset of S whose union is S. bellNumbers only
        /// takes integer arguments. The following condition must be enforced: n
        /// >= 0
        abstract bellNumbers: this: MathJsChain<float> -> MathJsChain<float>
        abstract bellNumbers: this: MathJsChain<BigNumber> -> MathJsChain<BigNumber>
        /// The Catalan Numbers enumerate combinatorial structures of many
        /// different types. catalan only takes integer arguments. The following
        /// condition must be enforced: n >= 0
        abstract catalan: this: MathJsChain<float> -> MathJsChain<float>
        abstract catalan: this: MathJsChain<BigNumber> -> MathJsChain<BigNumber>
        /// <summary>
        /// The composition counts of n into k parts. Composition only takes
        /// integer arguments. The following condition must be enforced: k &lt;= n.
        /// </summary>
        /// <param name="k">Number of objects in the subset</param>
        abstract composition: this: MathJsChain<'T> * k: U2<float, BigNumber> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// The Stirling numbers of the second kind, counts the number of ways to
        /// partition a set of n labelled objects into k nonempty unlabelled
        /// subsets. stirlingS2 only takes integer arguments. The following
        /// condition must be enforced: k &lt;= n. If n = k or k = 1, then s(n,k) =
        /// 1
        /// </summary>
        /// <param name="k">Number of objects in the subset</param>
        abstract stirlingS2: this: MathJsChain<'T> * k: U2<float, BigNumber> -> MathJsChain<NoLiteralType<'T>>
        /// Compute the argument of a complex value. For a complex number a + bi,
        /// the argument is computed as atan2(b, a). For matrices, the function
        /// is evaluated element wise.
        abstract arg: this: MathJsChain<U2<float, Complex>> -> MathJsChain<float>
        abstract arg: this: MathJsChain<U2<BigNumber, Complex>> -> MathJsChain<BigNumber>
        abstract arg: this: MathJsChain<MathArray> -> MathJsChain<MathArray>
        abstract arg: this: MathJsChain<Matrix> -> MathJsChain<Matrix>
        /// Compute the complex conjugate of a complex value. If x = a+bi, the
        /// complex conjugate of x is a - bi. For matrices, the function is
        /// evaluated element wise.
        abstract conj: this: MathJsChain<'T> -> MathJsChain<NoLiteralType<'T>>
        /// Get the imaginary part of a complex number. For a complex number a +
        /// bi, the function returns b. For matrices, the function is evaluated
        /// element wise.
        abstract im: this: MathJsChain<U2<float, Complex>> -> MathJsChain<float>
        abstract im: this: MathJsChain<BigNumber> -> MathJsChain<BigNumber>
        abstract im: this: MathJsChain<MathCollection> -> MathJsChain<MathCollection>
        /// Get the real part of a complex number. For a complex number a + bi,
        /// the function returns a. For matrices, the function is evaluated
        /// element wise.
        abstract re: this: MathJsChain<U2<float, Complex>> -> MathJsChain<float>
        abstract re: this: MathJsChain<BigNumber> -> MathJsChain<BigNumber>
        abstract re: this: MathJsChain<MathCollection> -> MathJsChain<MathCollection>
        /// <summary>
        /// Calculates: The eucledian distance between two points in 2 and 3
        /// dimensional spaces. Distance between point and a line in 2 and 3
        /// dimensional spaces. Pairwise distance between a set of 2D or 3D
        /// points NOTE: When substituting coefficients of a line(a, b and c),
        /// use ax + by + c = 0 instead of ax + by = c For parametric equation of
        /// a 3D line, x0, y0, z0, a, b, c are from: (x−x0, y−y0, z−z0) = t(a, b,
        /// c)
        /// </summary>
        /// <param name="y">Coordinates of the second point</param>
        abstract distance: this: MathJsChain<U2<MathCollection, obj>> * y: U2<MathCollection, obj> -> MathJsChain<U2<float, BigNumber>>
        /// <summary>
        /// Calculates the point of intersection of two lines in two or three
        /// dimensions and of a line and a plane in three dimensions. The inputs
        /// are in the form of arrays or 1 dimensional matrices. The line
        /// intersection functions return null if the lines do not meet. Note:
        /// Fill the plane coefficients as x + y + z = c and not as x + y + z + c
        /// = 0.
        /// </summary>
        /// <param name="x">Co-ordinates of second end-point of first line</param>
        /// <param name="y">
        /// Co-ordinates of first end-point of second line OR
        /// Coefficients of the plane's equation
        /// </param>
        /// <param name="z">
        /// Co-ordinates of second end-point of second line OR null if
        /// the calculation is for line and plane
        /// </param>
        abstract intersect: this: MathJsChain<MathCollection> * x: MathCollection * y: MathCollection * ?z: MathCollection -> MathJsChain<MathArray>
        /// <summary>
        /// Logical and. Test whether two values are both defined with a
        /// nonzero/nonempty value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="y">Second value to and</param>
        abstract ``and``: this: MathJsChain<U5<float, BigNumber, Complex, Unit, MathCollection>> * y: U5<float, BigNumber, Complex, Unit, MathCollection> -> MathJsChain<U2<bool, MathCollection>>
        /// Logical not. Flips boolean value of a given parameter. For matrices,
        /// the function is evaluated element wise.
        abstract not: this: MathJsChain<U5<float, BigNumber, Complex, Unit, MathCollection>> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Logical or. Test if at least one value is defined with a
        /// nonzero/nonempty value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="y">Second value to or</param>
        abstract ``or``: this: MathJsChain<U5<float, BigNumber, Complex, Unit, MathCollection>> * y: U5<float, BigNumber, Complex, Unit, MathCollection> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Logical xor. Test whether one and only one value is defined with a
        /// nonzero/nonempty value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="y">Second value to xor</param>
        abstract xor: this: MathJsChain<U5<float, BigNumber, Complex, Unit, MathCollection>> * y: U5<float, BigNumber, Complex, Unit, MathCollection> -> MathJsChain<U2<bool, MathCollection>>
        /// Concatenate two or more matrices. dim: number is a zero-based
        /// dimension over which to concatenate the matrices. By default the last
        /// dimension of the matrices.
        abstract concat: this: MathJsChain<Array<U3<MathCollection, float, BigNumber>>> -> MathJsChain<MathCollection>
        /// <summary>
        /// Calculate the cross product for two vectors in three dimensional
        /// space. The cross product of A = [a1, a2, a3] and B =[b1, b2, b3] is
        /// defined as: cross(A, B) = [ a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1
        /// * b2 - a2 * b1 ]
        /// </summary>
        /// <param name="y">Second vector</param>
        abstract cross: this: MathJsChain<MathCollection> * y: MathCollection -> MathJsChain<MathCollection>
        /// Calculate the determinant of a matrix.
        abstract det: this: MathJsChain<MathCollection> -> MathJsChain<float>
        /// <summary>
        /// Create a diagonal matrix or retrieve the diagonal of a matrix. When x
        /// is a vector, a matrix with vector x on the diagonal will be returned.
        /// When x is a two dimensional matrix, the matrixes kth diagonal will be
        /// returned as vector. When k is positive, the values are placed on the
        /// super diagonal. When k is negative, the values are placed on the sub
        /// diagonal.
        /// </summary>
        /// <param name="k">
        /// The diagonal where the vector will be filled in or
        /// retrieved. Default value: 0.
        /// </param>
        /// <param name="format">The matrix storage format. Default value: 'dense'.</param>
        abstract diag: this: MathJsChain<MathCollection> * ?format: string -> MathJsChain<Matrix>
        abstract diag: this: MathJsChain<MathCollection> * k: U2<float, BigNumber> * ?format: string -> MathJsChain<MathCollection>
        /// <summary>
        /// Calculate the dot product of two vectors. The dot product of A = [a1,
        /// a2, a3, ..., an] and B = [b1, b2, b3, ..., bn] is defined as: dot(A,
        /// B) = a1 * b1 + a2 * b2 + a3 * b3 + ... + an * bn
        /// </summary>
        /// <param name="y">Second vector</param>
        abstract dot: this: MathJsChain<MathCollection> * y: MathCollection -> MathJsChain<float>
        /// Compute the matrix exponential, expm(A) = e^A. The matrix must be
        /// square. Not to be confused with exp(a), which performs element-wise
        /// exponentiation. The exponential is calculated using the Padé
        /// approximant with scaling and squaring; see “Nineteen Dubious Ways to
        /// Compute the Exponential of a Matrix,” by Moler and Van Loan.
        abstract expm: this: MathJsChain<Matrix> -> MathJsChain<Matrix>
        /// <summary>
        /// Performs a real Schur decomposition of the real matrix A = UTU' where U is orthogonal
        /// and T is upper quasi-triangular.
        /// <see href="https://en.wikipedia.org/wiki/Schur_decomposition" />
        /// </summary>
        /// <returns>Object containing both matrix U and T of the Schur Decomposition A=UTU'</returns>
        abstract schur: this: MathJsChain<MathCollection> -> SchurDecomposition
        /// <summary>
        /// Solves the Continuous-time Lyapunov equation AP+PA'=Q for P, where Q is a positive semidefinite
        /// matrix.
        /// <see href="https://en.wikipedia.org/wiki/Lyapunov_equation" />
        /// </summary>
        /// <param name="Q">Matrix Q</param>
        /// <returns>Matrix P solution to the Continuous-time Lyapunov equation AP+PA'=Q</returns>
        abstract lyap: this: MathJsChain<MathCollection> * Q: MathCollection -> MathJsChain<MathCollection>
        /// <summary>
        /// Create a 2-dimensional identity matrix with size m x n or n x n. The
        /// matrix has ones on the diagonal and zeros elsewhere.
        /// </summary>
        /// <param name="format">The Matrix storage format</param>
        abstract identity: this: MathJsChain<U3<float, ResizeArray<float>, MathCollection>> * ?format: string -> MathJsChain<U2<MathCollection, float>>
        /// <param name="n">The y dimension for the matrix</param>
        /// <param name="format">The Matrix storage format</param>
        abstract identity: this: MathJsChain<float> * n: float * ?format: string -> MathJsChain<U2<MathCollection, float>>
        /// Filter the items in an array or one dimensional matrix.
        abstract filter: this: MathJsChain<U2<MathCollection, ResizeArray<string>>> * test: U2<(obj option -> obj option -> U2<MathCollection, ResizeArray<string>> -> bool), RegExp> -> MathJsChain<MathCollection>
        /// Flatten a multi dimensional matrix into a single dimensional matrix.
        abstract flatten: x: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// Iterate over all elements of a matrix/array, and executes the given
        /// callback function.
        abstract forEach: this: MathJsChain<'T> * callback: (obj option -> obj option -> 'T -> unit) -> unit when 'T :> MathCollection
        /// Calculate the inverse of a square matrix.
        abstract inv: this: MathJsChain<'T> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>Calculate the kronecker product of two matrices or vectors</summary>
        /// <param name="y">Second vector</param>
        abstract kron: this: MathJsChain<MathCollection> * y: MathCollection -> MathJsChain<Matrix>
        /// <summary>
        /// Iterate over all elements of a matrix/array, and executes the given
        /// callback function.
        /// </summary>
        /// <param name="callback">
        /// The callback function is invoked with three
        /// parameters: the value of the element, the index of the element, and
        /// the Matrix/array being traversed.
        /// </param>
        abstract map: this: MathJsChain<'T> * callback: (obj option -> obj option -> 'T -> U2<MathType, string>) -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Create a matrix filled with ones. The created matrix can have one or
        /// multiple dimensions.
        /// </summary>
        /// <param name="format">The matrix storage format</param>
        abstract ones: this: MathJsChain<U4<float, ResizeArray<float>, BigNumber, ResizeArray<BigNumber>>> * ?format: string -> MathJsChain<MathCollection>
        /// <summary>
        /// Partition-based selection of an array or 1D matrix. Will find the kth
        /// smallest value, and mutates the input array. Uses Quickselect.
        /// </summary>
        /// <param name="k">The kth smallest value to be retrieved; zero-based index</param>
        /// <param name="compare">
        /// An optional comparator function. The function is
        /// called as compare(a, b), and must return 1 when a &gt; b, -1 when a &lt; b,
        /// and 0 when a == b. Default value: 'asc'.
        /// </param>
        abstract partitionSelect: this: MathJsChain<MathCollection> * k: float * ?compare: U2<(obj option -> obj option -> float), string> -> MathJsChain<MathCollection>
        /// <summary>
        /// Create an array from a range. By default, the range end is excluded.
        /// This can be customized by providing an extra parameter includeEnd.
        /// </summary>
        /// <param name="end">
        /// End of the range, excluded by default, included when
        /// parameter includeEnd=true
        /// </param>
        /// <param name="step">Step size. Default value is 1.</param>
        /// <param name="includeEnd">
        /// Option to specify whether to include the end or
        /// not. False by default
        /// </param>
        abstract range: this: MathJsChain<string> * ?includeEnd: bool -> MathJsChain<Matrix>
        abstract range: this: MathJsChain<U2<float, BigNumber>> * ``end``: U2<float, BigNumber> * ?includeEnd: bool -> MathJsChain<Matrix>
        abstract range: this: MathJsChain<U2<float, BigNumber>> * ``end``: U2<float, BigNumber> * step: U2<float, BigNumber> * ?includeEnd: bool -> MathJsChain<Matrix>
        /// <summary>Reshape a multi dimensional array to fit the specified dimensions</summary>
        /// <param name="sizes">
        /// One dimensional array with integral sizes for each
        /// dimension
        /// </param>
        abstract reshape: this: MathJsChain<'T> * sizes: ResizeArray<float> -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>Resize a matrix</summary>
        /// <param name="size">One dimensional array with numbers</param>
        /// <param name="defaultValue">
        /// Zero by default, except in case of a string, in
        /// that case defaultValue = ' ' Default value: 0.
        /// </param>
        abstract resize: this: MathJsChain<'T> * size: MathCollection * ?defaultValue: U2<float, string> -> MathJsChain<'T> when 'T :> MathCollection
        /// Calculate the size of a matrix or scalar.
        abstract size: this: MathJsChain<U6<bool, float, Complex, Unit, string, MathCollection>> -> MathJsChain<MathCollection>
        /// <summary>Sort the items in a matrix</summary>
        /// <param name="compare">
        /// An optional _comparator function or name. The function
        /// is called as compare(a, b), and must return 1 when a &gt; b, -1 when a &lt;
        /// b, and 0 when a == b. Default value: ‘asc’
        /// </param>
        abstract sort: this: MathJsChain<'T> * compare: U2<(obj option -> obj option -> float), string> -> MathJsChain<'T> when 'T :> MathCollection
        /// Calculate the principal square root of a square matrix. The principal
        /// square root matrix X of another matrix A is such that X * X = A.
        abstract sqrtm: A: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// Squeeze a matrix, remove inner and outer singleton dimensions from a
        /// matrix.
        abstract squeeze: x: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>Get or set a subset of a matrix or string.</summary>
        /// <param name="index">For each dimension, an index or list of indices to get or set</param>
        /// <param name="replacement">
        /// An array, matrix, or scalar. If provided, the
        /// subset is replaced with replacement. If not provided, the subset is
        /// returned
        /// </param>
        /// <param name="defaultValue">
        /// Default value, filled in on new entries when the
        /// matrix is resized. If not provided, math.matrix elements will be left
        /// undefined. Default value: undefined.
        /// </param>
        abstract subset: this: MathJsChain<'T> * index: Index * ?replacement: obj * ?defaultValue: obj -> MathJsChain<'T>
        /// Calculate the trace of a matrix: the sum of the elements on the main
        /// diagonal of a square matrix.
        abstract trace: this: MathJsChain<MathCollection> -> MathJsChain<float>
        /// Transpose a matrix. All values of the matrix are reflected over its
        /// main diagonal. Only two dimensional matrices are supported.
        abstract transpose: x: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Create a matrix filled with zeros. The created matrix can have one or
        /// multiple dimensions.
        /// </summary>
        /// <param name="format">The matrix storage format</param>
        /// <returns>A matrix filled with zeros</returns>
        abstract zeros: this: MathJsChain<U4<float, ResizeArray<float>, BigNumber, ResizeArray<BigNumber>>> * ?format: string -> MathJsChain<MathCollection>
        /// <summary>
        /// Compute the number of ways of picking k unordered outcomes from n
        /// possibilities. Combinations only takes integer arguments. The
        /// following condition must be enforced: k &lt;= n.
        /// </summary>
        /// <param name="k">Number of objects in the subset</param>
        abstract combinations: n: MathJsChain<'T> * k: U2<float, BigNumber> -> MathJsChain<NoLiteralType<'T>>
        /// Compute the factorial of a value Factorial only supports an integer
        /// value as argument. For matrices, the function is evaluated element
        /// wise.
        abstract factorial: n: MathJsChain<'T> -> MathJsChain<NoLiteralType<'T>>
        /// Compute the gamma function of a value using Lanczos approximation for
        /// small values, and an extended Stirling approximation for large
        /// values. For matrices, the function is evaluated element wise.
        abstract gamma: n: MathJsChain<'T> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Calculate the Kullback-Leibler (KL) divergence between two
        /// distributions
        /// </summary>
        /// <param name="p">Second vector</param>
        abstract kldivergence: this: MathJsChain<MathCollection> * p: MathCollection -> MathJsChain<float>
        /// Multinomial Coefficients compute the number of ways of picking a1,
        /// a2, ..., ai unordered outcomes from n possibilities. multinomial
        /// takes one array of integers as an argument. The following condition
        /// must be enforced: every ai <= 0
        abstract multinomial: a: MathJsChain<ResizeArray<'T>> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Compute the number of ways of obtaining an ordered subset of k
        /// elements from a set of n elements. Permutations only takes integer
        /// arguments. The following condition must be enforced: k &lt;= n.
        /// </summary>
        /// <param name="k">The number of objects in the subset</param>
        abstract permutations: n: MathJsChain<'T> * ?k: U2<float, BigNumber> -> MathJsChain<NoLiteralType<'T>>
        /// <summary>
        /// Random pick a value from a one dimensional array. Array element is
        /// picked using a random function with uniform distribution.
        /// </summary>
        /// <param name="number">An int or float</param>
        /// <param name="weights">An array of ints or floats</param>
        abstract pickRandom: this: MathJsChain<ResizeArray<'T>> -> MathJsChain<'T>
        abstract pickRandom: this: MathJsChain<ResizeArray<'T>> * number: float -> MathJsChain<ResizeArray<'T>>
        abstract pickRandom: this: MathJsChain<ResizeArray<'T>> * number: float * weights: ResizeArray<float> -> MathJsChain<ResizeArray<'T>>
        /// <summary>
        /// Return a random number larger or equal to min and smaller than max
        /// using a uniform distribution.
        /// </summary>
        /// <param name="min">Minimum boundary for the random value, included</param>
        /// <param name="max">Maximum boundary for the random value, excluded</param>
        abstract random: this: MathJsChain<float> * ?max: float -> MathJsChain<float>
        abstract random: this: MathJsChain<'T> * ?min: float * ?max: float -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Return a random integer number larger or equal to min and smaller
        /// than max using a uniform distribution.
        /// </summary>
        /// <param name="min">Minimum boundary for the random value, included</param>
        /// <param name="max">Maximum boundary for the random value, excluded</param>
        abstract randomInt: this: MathJsChain<'T> * ?max: float -> MathJsChain<'T> when 'T :> MathCollection
        abstract randomInt: this: MathJsChain<'T> * min: float * max: float -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Compare two values. Returns 1 when x &gt; y, -1 when x &lt; y, and 0 when x
        /// == y. x and y are considered equal when the relative difference
        /// between x and y is smaller than the configured epsilon. The function
        /// cannot be used to compare values smaller than approximately 2.22e-16.
        /// For matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to compare</param>
        abstract compare: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U4<float, BigNumber, Fraction, MathCollection>>
        /// <summary>
        /// Compare two values of any type in a deterministic, natural way. For
        /// numeric values, the function works the same as math.compare. For
        /// types of values that can’t be compared mathematically, the function
        /// compares in a natural way.
        /// </summary>
        /// <param name="y">Second value to compare</param>
        abstract compareNatural: this: MathJsChain<obj option> * y: obj option -> MathJsChain<float>
        /// <summary>
        /// Compare two strings lexically. Comparison is case sensitive. Returns
        /// 1 when x &gt; y, -1 when x &lt; y, and 0 when x == y. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second string to compare</param>
        abstract compareText: this: MathJsChain<U2<string, MathCollection>> * y: U2<string, MathCollection> -> MathJsChain<U2<float, MathCollection>>
        /// <summary>
        /// Test element wise whether two matrices are equal. The function
        /// accepts both matrices and scalar values.
        /// </summary>
        /// <param name="y">Second amtrix to compare</param>
        abstract deepEqual: this: MathJsChain<MathType> * y: MathType -> MathJsChain<MathType>
        /// <summary>
        /// Test whether two values are equal.
        /// 
        /// The function tests whether the relative difference between x and y is
        /// smaller than the configured epsilon. The function cannot be used to
        /// compare values smaller than approximately 2.22e-16. For matrices, the
        /// function is evaluated element wise. In case of complex numbers, x.re
        /// must equal y.re, and x.im must equal y.im. Values null and undefined
        /// are compared strictly, thus null is only equal to null and nothing
        /// else, and undefined is only equal to undefined and nothing else.
        /// </summary>
        /// <param name="y">Second value to compare</param>
        abstract equal: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Check equality of two strings. Comparison is case sensitive. For
        /// matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second string to compare</param>
        abstract equalText: this: MathJsChain<U2<string, MathCollection>> * y: U2<string, MathCollection> -> MathJsChain<U2<float, MathCollection>>
        /// <summary>
        /// Test whether value x is larger than y. The function returns true when
        /// x is larger than y and the relative difference between x and y is
        /// larger than the configured epsilon. The function cannot be used to
        /// compare values smaller than approximately 2.22e-16. For matrices, the
        /// function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to compare</param>
        abstract larger: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Test whether value x is larger or equal to y. The function returns
        /// true when x is larger than y or the relative difference between x and
        /// y is smaller than the configured epsilon. The function cannot be used
        /// to compare values smaller than approximately 2.22e-16. For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to vcompare</param>
        abstract largerEq: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Test whether value x is smaller than y. The function returns true
        /// when x is smaller than y and the relative difference between x and y
        /// is smaller than the configured epsilon. The function cannot be used
        /// to compare values smaller than approximately 2.22e-16. For matrices,
        /// the function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to vcompare</param>
        abstract smaller: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Test whether value x is smaller or equal to y. The function returns
        /// true when x is smaller than y or the relative difference between x
        /// and y is smaller than the configured epsilon. The function cannot be
        /// used to compare values smaller than approximately 2.22e-16. For
        /// matrices, the function is evaluated element wise.
        /// </summary>
        /// <param name="y">Second value to compare</param>
        abstract smallerEq: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Test whether two values are unequal. The function tests whether the
        /// relative difference between x and y is larger than the configured
        /// epsilon. The function cannot be used to compare values smaller than
        /// approximately 2.22e-16. For matrices, the function is evaluated
        /// element wise. In case of complex numbers, x.re must unequal y.re, or
        /// x.im must unequal y.im. Values null and undefined are compared
        /// strictly, thus null is unequal with everything except null, and
        /// undefined is unequal with everything except undefined.
        /// </summary>
        /// <param name="y">Second value to vcompare</param>
        abstract unequal: this: MathJsChain<U2<MathType, string>> * y: U2<MathType, string> -> MathJsChain<U2<bool, MathCollection>>
        /// <summary>
        /// Create the cartesian product of two (multi)sets. Multi-dimension
        /// arrays will be converted to single-dimension arrays and the values
        /// will be sorted in ascending order before the operation.
        /// </summary>
        /// <param name="a2">A (multi)set</param>
        abstract setCartesian: this: MathJsChain<'T> * a2: MathCollection -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Create the difference of two (multi)sets: every element of set1, that
        /// is not the element of set2. Multi-dimension arrays will be converted
        /// to single-dimension arrays before the operation
        /// </summary>
        /// <param name="a2">A (multi)set</param>
        abstract setDifference: this: MathJsChain<'T> * a2: MathCollection -> MathJsChain<'T> when 'T :> MathCollection
        /// Collect the distinct elements of a multiset. A multi-dimension array
        /// will be converted to a single-dimension array before the operation.
        abstract setDistinct: a: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Create the intersection of two (multi)sets. Multi-dimension arrays
        /// will be converted to single-dimension arrays before the operation.
        /// </summary>
        /// <param name="a2">A (multi)set</param>
        abstract setIntersect: this: MathJsChain<'T> * a2: MathCollection -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Check whether a (multi)set is a subset of another (multi)set. (Every
        /// element of set1 is the element of set2.) Multi-dimension arrays will
        /// be converted to single-dimension arrays before the operation.
        /// </summary>
        /// <param name="a2">A (multi)set</param>
        abstract setIsSubset: this: MathJsChain<MathCollection> * a2: MathCollection -> MathJsChain<bool>
        /// <summary>
        /// Count the multiplicity of an element in a multiset. A multi-dimension
        /// array will be converted to a single-dimension array before the
        /// operation.
        /// </summary>
        /// <param name="a">A multiset</param>
        abstract setMultiplicity: e: MathJsChain<MathNumericType> * a: MathCollection -> MathJsChain<float>
        /// Create the powerset of a (multi)set. (The powerset contains very
        /// possible subsets of a (multi)set.) A multi-dimension array will be
        /// converted to a single-dimension array before the operation.
        abstract setPowerset: a: MathJsChain<'T> -> MathJsChain<'T> when 'T :> MathCollection
        /// Count the number of elements of a (multi)set. When a second parameter
        /// is ‘true’, count only the unique values. A multi-dimension array will
        /// be converted to a single-dimension array before the operation.
        abstract setSize: this: MathJsChain<MathCollection> -> MathJsChain<float>
        /// <summary>
        /// Create the symmetric difference of two (multi)sets. Multi-dimension
        /// arrays will be converted to single-dimension arrays before the
        /// operation.
        /// </summary>
        /// <param name="a2">A (multi)set</param>
        abstract setSymDifference: this: MathJsChain<'T> * a2: MathCollection -> MathJsChain<'T> when 'T :> MathCollection
        /// <summary>
        /// Create the union of two (multi)sets. Multi-dimension arrays will be
        /// converted to single-dimension arrays before the operation.
        /// </summary>
        /// <param name="a2">A (multi)set</param>
        abstract setUnion: this: MathJsChain<'T> * a2: MathCollection -> MathJsChain<'T> when 'T :> MathCollection
        /// Compute the erf function of a value using a rational Chebyshev
        /// approximations for different intervals of x.
        abstract erf: this: MathJsChain<'T> -> MathJsChain<NoLiteralType<'T>>
        /// Compute the median absolute deviation of a matrix or a list with
        /// values. The median absolute deviation is defined as the median of the
        /// absolute deviations from the median.
        abstract mad: this: MathJsChain<MathCollection> -> MathJsChain<obj option>
        /// <summary>
        /// Compute the maximum value of a matrix or a list with values. In case
        /// of a multi dimensional array, the maximum of the flattened array will
        /// be calculated. When dim is provided, the maximum over the selected
        /// dimension will be calculated. Parameter dim is zero-based.
        /// </summary>
        /// <param name="dim">The maximum over the selected dimension</param>
        abstract max: this: MathJsChain<ResizeArray<MathType>> * ?dim: float -> MathJsChain<obj option>
        abstract max: this: MathJsChain<MathCollection> * ?dim: float -> MathJsChain<obj option>
        /// <summary>
        /// Compute the mean value of matrix or a list with values. In case of a
        /// multi dimensional array, the mean of the flattened array will be
        /// calculated. When dim is provided, the maximum over the selected
        /// dimension will be calculated. Parameter dim is zero-based.
        /// </summary>
        /// <param name="dim">The mean over the selected dimension</param>
        abstract mean: this: MathJsChain<ResizeArray<MathType>> * ?dim: float -> MathJsChain<obj option>
        abstract mean: this: MathJsChain<MathCollection> * ?dim: float -> MathJsChain<obj option>
        /// Compute the median of a matrix or a list with values. The values are
        /// sorted and the middle value is returned. In case of an even number of
        /// values, the average of the two middle values is returned. Supported
        /// types of values are: Number, BigNumber, Unit In case of a (multi
        /// dimensional) array or matrix, the median of all elements will be
        /// calculated.
        abstract median: this: MathJsChain<ResizeArray<MathType>> * ?dim: float -> MathJsChain<obj option>
        abstract median: this: MathJsChain<MathCollection> * ?dim: float -> MathJsChain<obj option>
        /// <summary>
        /// Compute the minimum value of a matrix or a list of values. In case of
        /// a multi dimensional array, the minimum of the flattened array will be
        /// calculated. When dim is provided, the minimum over the selected
        /// dimension will be calculated. Parameter dim is zero-based.
        /// </summary>
        /// <param name="dim">The minimum over the selected dimension</param>
        abstract min: this: MathJsChain<ResizeArray<MathType>> -> MathJsChain<ResizeArray<MathType>>
        abstract min: this: MathJsChain<MathCollection> * ?dim: float -> MathJsChain<obj option>
        /// Computes the mode of a set of numbers or a list with values(numbers
        /// or characters). If there are more than one modes, it returns a list
        /// of those values.
        abstract mode: this: MathJsChain<ResizeArray<MathType>> -> MathJsChain<ResizeArray<MathType>>
        /// Compute the product of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the sum of all elements will be
        /// calculated.
        abstract prod: this: MathJsChain<ResizeArray<MathType>> -> MathJsChain<obj option>
        /// <summary>
        /// Compute the prob order quantile of a matrix or a list with values.
        /// The sequence is sorted and the middle value is returned. Supported
        /// types of sequence values are: Number, BigNumber, Unit Supported types
        /// of probability are: Number, BigNumber In case of a (multi
        /// dimensional) array or matrix, the prob order quantile of all elements
        /// will be calculated.
        /// </summary>
        /// <param name="probOrN">
        /// prob is the order of the quantile, while N is the
        /// amount of evenly distributed steps of probabilities; only one of
        /// these options can be provided
        /// </param>
        /// <param name="sorted">=false is data sorted in ascending order</param>
        abstract quantileSeq: A: MathJsChain<MathCollection> * prob: U3<float, BigNumber, MathArray> * ?sorted: bool -> MathJsChain<U4<float, BigNumber, Unit, MathArray>>
        /// <summary>
        /// Compute the standard deviation of a matrix or a list with values. The
        /// standard deviations is defined as the square root of the variance:
        /// std(A) = sqrt(variance(A)). In case of a (multi dimensional) array or
        /// matrix, the standard deviation over all elements will be calculated.
        /// Optionally, the type of normalization can be specified as second
        /// parameter. The parameter normalization can be one of the following
        /// values: 'unbiased' (default) The sum of squared errors is divided by
        /// (n - 1) 'uncorrected' The sum of squared errors is divided by n
        /// 'biased' The sum of squared errors is divided by (n + 1)
        /// </summary>
        /// <param name="dim">A dimension to compute standard deviation.</param>
        /// <param name="normalization">
        /// Determines how to normalize the variance. Choose
        /// ‘unbiased’ (default), ‘uncorrected’, or ‘biased’. Default value:
        /// ‘unbiased’.
        /// </param>
        /// <returns>The standard deviation</returns>
        abstract std: this: MathJsChain<ResizeArray<float>> * ?dim: float * ?normalization: MathJsStaticStd -> MathJsChain<float>
        /// <summary>
        /// Compute the standard deviation of a matrix or a list with values. The
        /// standard deviations is defined as the square root of the variance:
        /// std(A) = sqrt(variance(A)). In case of a (multi dimensional) array or
        /// matrix, the standard deviation over all elements will be calculated.
        /// Optionally, the type of normalization can be specified as second
        /// parameter. The parameter normalization can be one of the following
        /// values: 'unbiased' (default) The sum of squared errors is divided by
        /// (n - 1) 'uncorrected' The sum of squared errors is divided by n
        /// 'biased' The sum of squared errors is divided by (n + 1)
        /// </summary>
        /// <param name="normalization">
        /// Determines how to normalize the variance. Choose
        /// ‘unbiased’ (default), ‘uncorrected’, or ‘biased’. Default value:
        /// ‘unbiased’.
        /// </param>
        /// <returns>The standard deviation</returns>
        abstract std: this: MathJsChain<MathCollection> * ?dimension: float * ?normalization: MathJsStaticStd -> MathJsChain<ResizeArray<float>>
        /// Compute the sum of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the sum of all elements will be
        /// calculated.
        abstract std: this: MathJsChain<MathCollection> * normalization: MathJsStaticStd -> MathJsChain<float>
        /// <summary>
        /// Compute the variance of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the variance over all elements
        /// will be calculated. Optionally, the type of normalization can be
        /// specified as second parameter. The parameter normalization can be one
        /// of the following values: 'unbiased' (default) The sum of squared
        /// errors is divided by (n - 1) 'uncorrected' The sum of squared errors
        /// is divided by n 'biased' The sum of squared errors is divided by (n +
        /// 1) Note that older browser may not like the variable name var. In
        /// that case, the function can be called as math<see cref="...">'var'</see> instead of
        /// math.variance(...).
        /// </summary>
        /// <param name="dim">a dimension to compute variance.</param>
        /// <param name="normalization">
        /// normalization Determines how to normalize the
        /// variance. Choose ‘unbiased’ (default), ‘uncorrected’, or ‘biased’.
        /// Default value: ‘unbiased’.
        /// </param>
        /// <returns>The variance</returns>
        abstract variance: this: MathJsChain<Array<Array<U3<float, BigNumber, Fraction>>>> -> MathJsChain<float>
        /// <summary>
        /// Compute the variance of a matrix or a list with values. In case of a
        /// (multi dimensional) array or matrix, the variance over all elements
        /// will be calculated. Optionally, the type of normalization can be
        /// specified as second parameter. The parameter normalization can be one
        /// of the following values: 'unbiased' (default) The sum of squared
        /// errors is divided by (n - 1) 'uncorrected' The sum of squared errors
        /// is divided by n 'biased' The sum of squared errors is divided by (n +
        /// 1) Note that older browser may not like the variable name var. In
        /// that case, the function can be called as math<see cref="...">'var'</see> instead of
        /// math.variance(...).
        /// </summary>
        /// <param name="normalization">
        /// normalization Determines how to normalize the
        /// variance. Choose ‘unbiased’ (default), ‘uncorrected’, or ‘biased’.
        /// Default value: ‘unbiased’.
        /// </param>
        /// <returns>The variance</returns>
        abstract variance: this: MathJsChain<MathCollection> * ?dimension: float * ?normalization: MathJsStaticStd -> MathJsChain<ResizeArray<float>>
        abstract variance: this: MathJsChain<MathCollection> * normalization: MathJsStaticStd -> MathJsChain<float>
        /// <summary>Format a value of any type into a string.</summary>
        /// <param name="options">An object with formatting options.</param>
        /// <param name="callback">
        /// A custom formatting function, invoked for all numeric
        /// elements in value, for example all elements of a matrix, or the real
        /// and imaginary parts of a complex number. This callback can be used to
        /// override the built-in numeric notation with any type of formatting.
        /// Function callback is called with value as parameter and must return a
        /// string.
        /// </param>
        /// <seealso href="http://mathjs.org/docs/reference/functions/format.html" />
        abstract format: this: MathJsChain<obj option> * value: obj option * ?options: U3<FormatOptions, float, (obj option -> string)> * ?callback: (obj option -> string) -> MathJsChain<string>
        /// <summary>Interpolate values into a string template.</summary>
        /// <param name="values">
        /// An object containing variables which will be filled in
        /// in the template.
        /// </param>
        /// <param name="precision">
        /// Number of digits to format numbers. If not provided,
        /// the value will not be rounded.
        /// </param>
        /// <param name="options">
        /// Formatting options, or the number of digits to format
        /// numbers. See function math.format for a description of all options.
        /// </param>
        abstract print: this: MathJsChain<string> * values: obj option * ?precision: float * ?options: U2<float, obj> -> MathJsChain<string>
        /// Calculate the inverse cosine of a value. For matrices, the function
        /// is evaluated element wise.
        abstract acos: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic arccos of a value, defined as acosh(x) =
        /// ln(sqrt(x^2 - 1) + x). For matrices, the function is evaluated
        /// element wise.
        abstract acosh: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the inverse cotangent of a value. For matrices, the
        /// function is evaluated element wise.
        abstract acot: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic arccotangent of a value, defined as acoth(x)
        /// = (ln((x+1)/x) + ln(x/(x-1))) / 2. For matrices, the function is
        /// evaluated element wise.
        abstract acoth: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the inverse cosecant of a value. For matrices, the function
        /// is evaluated element wise.
        abstract acsc: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic arccosecant of a value, defined as acsch(x)
        /// = ln(1/x + sqrt(1/x^2 + 1)). For matrices, the function is evaluated
        /// element wise.
        abstract acsch: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the inverse secant of a value. For matrices, the function
        /// is evaluated element wise.
        abstract asec: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic arcsecant of a value, defined as asech(x) =
        /// ln(sqrt(1/x^2 - 1) + 1/x). For matrices, the function is evaluated
        /// element wise.
        abstract asech: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the inverse sine of a value. For matrices, the function is
        /// evaluated element wise.
        abstract asin: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic arcsine of a value, defined as asinh(x) =
        /// ln(x + sqrt(x^2 + 1)). For matrices, the function is evaluated
        /// element wise.
        abstract asinh: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the inverse tangent of a value. For matrices, the function
        /// is evaluated element wise.
        abstract atan: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the inverse tangent function with two arguments, y/x. By
        /// providing two arguments, the right quadrant of the computed angle can
        /// be determined. For matrices, the function is evaluated element wise.
        abstract atan2: this: MathJsChain<'T> * x: float -> MathJsChain<'T>
        /// Calculate the hyperbolic arctangent of a value, defined as atanh(x) =
        /// ln((1 + x)/(1 - x)) / 2. For matrices, the function is evaluated
        /// element wise.
        abstract atanh: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the cosine of a value. For matrices, the function is
        /// evaluated element wise.
        abstract cos: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic cosine of a value, defined as cosh(x) = 1/2
        /// * (exp(x) + exp(-x)). For matrices, the function is evaluated element
        /// wise.
        abstract cosh: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the cotangent of a value. cot(x) is defined as 1 / tan(x).
        /// For matrices, the function is evaluated element wise.
        abstract cot: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic cotangent of a value, defined as coth(x) = 1
        /// / tanh(x). For matrices, the function is evaluated element wise.
        abstract coth: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the cosecant of a value, defined as csc(x) = 1/sin(x). For
        /// matrices, the function is evaluated element wise.
        abstract csc: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic cosecant of a value, defined as csch(x) = 1
        /// / sinh(x). For matrices, the function is evaluated element wise.
        abstract csch: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the secant of a value, defined as sec(x) = 1/cos(x). For
        /// matrices, the function is evaluated element wise.
        abstract sec: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic secant of a value, defined as sech(x) = 1 /
        /// cosh(x). For matrices, the function is evaluated element wise.
        abstract sech: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the sine of a value. For matrices, the function is
        /// evaluated element wise.
        abstract sin: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic sine of a value, defined as sinh(x) = 1/2 *
        /// (exp(x) - exp(-x)). For matrices, the function is evaluated element
        /// wise.
        abstract sinh: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the tangent of a value. tan(x) is equal to sin(x) / cos(x).
        /// For matrices, the function is evaluated element wise.
        abstract tan: this: MathJsChain<'T> -> MathJsChain<'T>
        /// Calculate the hyperbolic tangent of a value, defined as tanh(x) =
        /// (exp(2 * x) - 1) / (exp(2 * x) + 1). For matrices, the function is
        /// evaluated element wise.
        abstract tanh: this: MathJsChain<'T> -> MathJsChain<'T>
        /// <summary>
        /// Change the unit of a value. For matrices, the function is evaluated
        /// element wise.
        /// </summary>
        /// <param name="unit">
        /// New unit. Can be a string like "cm" or a unit without
        /// value.
        /// </param>
        abstract ``to``: this: MathJsChain<U2<Unit, MathCollection>> * unit: U2<Unit, string> -> MathJsChain<U2<Unit, MathCollection>>
        /// Clone an object.
        abstract clone: this: MathJsChain<'TValue> -> MathJsChain<'TValue>
        /// Test whether a value is an integer number. The function supports
        /// number, BigNumber, and Fraction. The function is evaluated
        /// element-wise in case of Array or Matrix input.
        abstract isInteger: this: MathJsChain<U4<float, BigNumber, Fraction, MathCollection>> -> MathJsChain<bool>
        /// Test whether a value is NaN (not a number). The function supports
        /// types number, BigNumber, Fraction, Unit and Complex. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        abstract isNaN: this: MathJsChain<U5<float, BigNumber, Fraction, MathCollection, Unit>> -> MathJsChain<bool>
        /// Test whether a value is negative: smaller than zero. The function
        /// supports types number, BigNumber, Fraction, and Unit. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        abstract isNegative: this: MathJsChain<U5<float, BigNumber, Fraction, MathCollection, Unit>> -> MathJsChain<bool>
        /// Test whether a value is an numeric value. The function is evaluated
        /// element-wise in case of Array or Matrix input.
        abstract isNumeric: this: MathJsChain<obj option> -> MathJsChain<bool>
        /// Test whether a value is positive: larger than zero. The function
        /// supports types number, BigNumber, Fraction, and Unit. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        abstract isPositive: this: MathJsChain<U5<float, BigNumber, Fraction, MathCollection, Unit>> -> MathJsChain<bool>
        /// Test whether a value is prime: has no divisors other than itself and
        /// one. The function supports type number, bignumber. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        abstract isPrime: this: MathJsChain<U3<float, BigNumber, MathCollection>> -> MathJsChain<bool>
        /// Test whether a value is zero. The function can check for zero for
        /// types number, BigNumber, Fraction, Complex, and Unit. The function is
        /// evaluated element-wise in case of Array or Matrix input.
        abstract isZero: this: MathJsChain<MathType> -> MathJsChain<bool>
        /// Determine the type of a variable.
        abstract typeOf: this: MathJsChain<obj option> -> MathJsChain<string>

    type [<AllowNullLiteral>] ImportOptions =
        abstract ``override``: bool option with get, set
        abstract silent: bool option with get, set
        abstract wrap: bool option with get, set

    type [<AllowNullLiteral>] ImportObject =
        [<EmitIndexer>] abstract Item: key: string -> obj option with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] MathJsStaticMatrix =
        | Sparse
        | Dense

    type [<StringEnum>] [<RequireQualifiedAccess>] MathJsStaticStd =
        | Unbiased
        | Uncorrected
        | Biased

    type [<AllowNullLiteral>] UnitComponentUnit =
        abstract name: string with get, set
        abstract ``base``: {| dimensions: ResizeArray<float>; key: string |} with get, set
        abstract prefixes: Record<string, UnitPrefix> with get, set
        abstract value: float with get, set
        abstract offset: float with get, set
        abstract dimensions: ResizeArray<float> with get, set

    type [<StringEnum>] [<RequireQualifiedAccess>] CreateUnitOptionsPrefixes =
        | None
        | Short
        | Long
        | Binary_short
        | Binary_long

    type [<StringEnum>] [<RequireQualifiedAccess>] FormatOptionsNotation =
        | Fixed
        | Exponential
        | Engineering
        | Auto

    type [<StringEnum>] [<RequireQualifiedAccess>] ConfigOptionsMatrix =
        | [<CompiledName("Matrix")>] Matrix
        | [<CompiledName("Array")>] Array

    type [<StringEnum>] [<RequireQualifiedAccess>] ConfigOptionsNumber =
        | Number
        | [<CompiledName("BigNumber")>] BigNumber
        | [<CompiledName("Fraction")>] Fraction