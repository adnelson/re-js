// Render a JS AST into JSON.
open Utils.Json.Encode;
open Ast;

let ident: encoder(ident) = object1("Ident", Identifier.toJson);

let synchronicity: encoder(synchronicity) =
  s =>
    switch (s) {
    | `Sync => "Sync"->string
    | `Async => "Async"->string
    };

let function_:
  'n 'b.
  (encoder('n), encoder('b)) => encoder(function_('n, 'b))
 =
  (nameToJson, bodyToJson, {sync, name, params, body}) =>
    object_([
      ("sync", sync |> synchronicity),
      ("name", name |> nameToJson),
      ("params", params |> array(ident)),
      ("body", body |> bodyToJson),
    ]);

let rec varDecKind: encoder(varDecKind) =
  k =>
    switch (k) {
    | `Var => "Var"->string
    | `Let => "Let"->string
    | `Const => "Const"->string
    }

and varDec: encoder(varDec) =
  ({kind, varName}) =>
    (kind, varName) |> object2("kind", varDecKind, "varName", ident)

and objectKey = ok =>
  switch (ok) {
  | `I(i) => i |> ident
  | `S(s) => s |> string
  | `E(e) => e |> expr
  }

and interpolatedStringPart = p =>
  switch (p) {
  | `S(s) => s |> object1("S", string)
  | `E(e) => e |> object1("E", expr)
  }

and expr: encoder(expr) =
  e =>
    switch (e) {
    | `This => "This"->string
    | `Null => "Null"->string
    | `Undefined => "Undefined"->string
    | `Bool(b) => b |> bool
    | `Number(f) => f |> float
    | `String(s) => s |> string
    | `InterpolatedString(parts) =>
      parts |> object1("InterpolatedString", array(interpolatedStringPart))
    | `Json(j) => j |> object1("Json", json)
    | `Variable(i) => i |> object1("Variable", ident)
    | `Array(es) => es |> object1("Array", array(expr))
    | `Object(kvs) => kvs |> object1("Object", array(pair(objectKey, expr)))
    | `Function(f) =>
      f |> object1("Function", function_(nullable(ident), block))
    | `ArrowFunction(f) =>
      f
      |> object1(
           "ArrowFunction",
           function_(
             () => null,
             body =>
               switch (body) {
               | `Block(ss) => ss |> array(statement)
               | `Return(e) => e |> object1("Return", expr)
               },
           ),
         )
    | `Class(optName, optExtends, properties) =>
      (optName, optExtends, properties)
      |> object1(
           "Class",
           tuple3(nullable(ident), nullable(expr), array(classProperty)),
         )
    | `Dot(e, i) => (e, i) |> object1("Dot", pair(expr, ident))
    | `Call(f, xs) => (f, xs) |> object1("Call", pair(expr, array(expr)))
    | `ArrayGet(x, y) => (x, y) |> object1("ArrayGet", pair(expr, expr))
    | `Binary(op, l, r) =>
      (op, l, r) |> object1("Binary", tuple3(string, expr, expr))
    | `Unary(op, e) => (op, e) |> object1("Unary", pair(string, expr))
    | `Ternary(cond, ifTrue, ifFalse) =>
      (cond, ifTrue, ifFalse)
      |> object1("Ternary", tuple3(expr, expr, expr))
    | `New(e) => e |> object1("New", expr)
    | `Jsx(node) => node |> object1("Jsx", jsxNode)
    | `Await(e) => e |> object1("Await", expr)
    }

and assignable: encoder(assignable) =
  a =>
    switch (a) {
    | `AssignVar(i) => i |> object1("AssignVar", ident)
    | `AssignObjectDot(e, i) =>
      (e, i) |> object1("AssignObjectDot", pair(expr, ident))
    | `AssignObjectBrackets(e, i) =>
      (e, i) |> object1("AssignObjectBrackets", pair(expr, expr))
    }

and block: encoder(block) =
  (`Block(ss)) => ss |> object1("Block", array(statement))

and staticness: encoder(staticness) =
  s =>
    switch (s) {
    | `Static => "Static"->string
    | `NotStatic => "NotStatic"->string
    }

and classProperty: encoder(classProperty) =
  cp =>
    switch (cp) {
    | `Function(s, f) =>
      (s, f)
      |> object1("Function", pair(staticness, function_(ident, block)))
    | `ClassVariable(n, e) =>
      (n, e) |> object1("ClassVariable", pair(ident, expr))
    }

and declaration: encoder(declaration) =
  d =>
    switch (d) {
    | `DeclareVar(vd, optE) =>
      (vd, optE) |> object1("DeclareVar", pair(varDec, nullable(expr)))
    | `DeclareFunction(f) =>
      f |> object1("DeclareFunction", function_(ident, block))
    | `DeclareClass(name, optExtends, properties) =>
      (name, optExtends, properties)
      |> object1(
           "DeclareClass",
           tuple3(ident, nullable(expr), array(classProperty)),
         )
    }

and statement: encoder(statement) =
  s =>
    switch (s) {
    | `Expr(e) => e |> object1("Expr", expr)
    | `If(e, ifTrue, optIfFalse) =>
      (e, ifTrue, optIfFalse)
      |> object3("cond", expr, "ifTrue", block, "ifFalse", nullable(block))
      |> object1("If", json)
    | `While(test, body) =>
      (test, body)
      |> object2("test", expr, "body", block)
      |> object1("While", json)
    | `For(v, init, iter, body) =>
      (v, init, iter, body)
      |> object4("var", varDec, "init", expr, "iter", expr, "body", block)
      |> object1("For", json)
    | `Declaration(dec) => dec |> object1("Declaration", declaration)
    | `Assign(lhs, e) =>
      (lhs, e) |> object1("Assign", pair(assignable, expr))
    | `Return(optE) => optE |> object1("Return", nullable(expr))
    | `Throw(e) => e |> object1("Throw", expr)
    | `Break => "Break"->string
    }

and jsxNode = n =>
  switch (n) {
  | `String(s) => s |> object1("String", string)
  | `Expr(e) => e |> object1("Expr", expr)
  | `Fragment(nodes) => nodes |> object1("Fragment", array(jsxNode))
  | `Element(elem) => elem |> object1("Element", jsxElement)
  }

and jsxElement = e =>
  object_([
    ("elementName", e.elementName |> ident),
    ("elementProps", e.elementProps |> array(pair(ident, expr))),
    ("elementChildren", e.elementChildren |> array(jsxNode)),
  ]);
