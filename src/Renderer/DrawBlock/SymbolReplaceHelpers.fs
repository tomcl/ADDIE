module SymbolReplaceHelpers


open CommonTypes
open Fable.React
open System.Text.RegularExpressions
open DrawModelType.SymbolT
open Symbol
open Optics
open Optic
open Operators
open System



/// Updates the value of a constant1 component and returns the updated symbol
let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Component.Type with
    //    | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    
    set (component_ >-> type_) newcompotype symbol

