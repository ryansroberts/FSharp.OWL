namespace FsRdf
open System.Linq
open System.Linq.Expressions
open System.Collections
open System.Collections.Generic
open System.Text
open System.Reflection

[<AutoOpen>]
module Helpers =
    let (|MethodWithName|_|) (s:string) (m:MethodInfo) =  if s = m.Name then Some () else None
    let (|PropertyWithName|_|) (s:string) (m:PropertyInfo) =  if s = m.Name then Some () else None

    let (|MethodCall|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Call, (:? MethodCallExpression as e) ->  
            Some ((match e.Object with null -> None | obj -> Some obj), e.Method, Seq.toList e.Arguments)
        | _ -> None

    let (|AsType|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.TypeAs, (:? UnaryExpression as e) ->  Some (e.Operand, e.Type)
        | _ -> None

    let (|NewArray|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.NewArrayInit, (:? NewArrayExpression as e) ->  Some (Seq.toList e.Expressions)
        | _ -> None

    let (|PropertyGet|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.MemberAccess, ( :? MemberExpression as e) -> 
            match e.Member with 
            | :? PropertyInfo as p -> 
                    Some ((match e.Expression with null -> None | obj -> Some obj), p)
            | _ -> None
        | _ -> None

    let (|Constant|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Constant, (:? ConstantExpression as ce) ->  Some (ce.Value, ce.Type)
        | _ -> None

    let (|String|_|) = function | Constant((:? string as s),_) -> Some s | _ -> None
    let (|Int32|_|) = function | Constant((:? int as s),_) -> Some s | _ -> None
    let (|Null|_|) = function Constant(null,_) -> Some () | _ -> None
    let (|Double|_|) = function Constant((:? double as s),_) -> Some s | _ -> None
    let (|Decimal|_|) = function Constant((:? decimal as s),_) -> Some s | _ -> None

    let (|Convert|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Convert, (:? UnaryExpression as ce) ->  Some (ce.Operand, ce.Type)
        | _ -> None
    
    let (|Var|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Parameter, (:? ParameterExpression as ce) ->  Some ce
        | _ -> None

    let (|Lambda|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Lambda, (:? LambdaExpression as ce) ->  Some (Seq.toList ce.Parameters, ce.Body)
        | _ -> None

    let (|LetExpr|_|) (e:Expression) = 
        match e with 
        | MethodCall(Some (Lambda([v],body)), m, [arg]) when m.Name = "Invoke" ->  Some(v,arg,body)
        | _ -> None

    let (|Quote|_|) (e:Expression) = 
        match e.NodeType, e with 
        | ExpressionType.Quote, (:? UnaryExpression as ce) -> Some ce.Operand
        | _ -> None

    let (|Select|_|) (e:Expression) =
        match e with
        | MethodCall(_, MethodWithName "Select", [body; iterVar]) -> Some (body, iterVar)
        | _ -> None
