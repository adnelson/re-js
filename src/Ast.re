type ident = Identifier.t

and varDecKind = [ | `Var | `Let | `Const]

and varDec = {
  kind: varDecKind,
  varName: ident,
}

and destructureObject = [
  | `Name(ident, option(ident))
  | `NameWithInner(ident, array(destructureObject))
  | `MultipleDestructures(array(destructureObject))
]

and assignable = [
  | `AssignVar(ident)
  | `AssignObjectDot(expr, ident)
  | `AssignObjectBrackets(expr, expr)
]

and block = [ | `Block(array(statement))]

and synchronicity = [ | `Sync | `Async]

// A function. The kind of function depends on type arguments:
// 'name is:
//   `option(ident)` for es5 functions
//   `ident` for class properties and module-level functions
//   `unit` for arrow functions
//
// 'body is:
//   `block` for non-arrow functions
//   `[ | `Block(block) | `Return(expr) ]` for arrow functions
//
and function_('name, 'body) = {
  sync: synchronicity,
  name: 'name,
  params: array(ident),
  body: 'body,
}

and staticness = [ | `Static | `NotStatic]

and classProperty = [
  | `Function(staticness, function_(ident, block))
  | `ClassVariable(ident, expr)
]

and declaration = [
  | `DeclareVar(varDec, option(expr))
  | `DeclareFunction(function_(ident, block))
  | `DeclareClass(ident, option(expr), array(classProperty))
]

and statement = [
  | `Expr(expr)
  | `If(expr, block, option(block))
  | `While(expr, block)
  | `For(varDec, expr, expr, block)
  | `Declaration(declaration)
  | `Assign(assignable, expr)
  | `Return(option(expr))
  | `Throw(expr)
  | `Break
]

and topLevelStatement = [ | `Statement(statement) | `Export(declaration)]

and importable = [ | `StarAs(ident) | `Destructure(destructureObject)]

and import = {
  what: array(importable),
  from: string,
}

and defaultExport = [ | `Declaration(declaration) | `ExportExpr(expr)]

and module_ = {
  imports: array(import),
  statements: array(topLevelStatement),
  defaultExport: option(defaultExport),
}

and jsxElement = {
  elementName: ident,
  elementProps: array((ident, expr)),
  elementChildren: array(jsxNode),
}

and jsxNode = [
  | `String(string)
  | `Expr(expr)
  | `Fragment(array(jsxNode))
  | `Element(jsxElement)
]

and interpolatedStringPart = [ | `S(string) | `E(expr)]

and objectKey = [ | `I(ident) | `S(string) | `E(expr)]

and expr = [
  | `This
  | `Null
  | `Undefined
  | `Bool(bool)
  | `Number(float)
  | `Variable(ident)
  | `String(string)
  | `InterpolatedString(array(interpolatedStringPart))
  | `Json(Js.Json.t)
  | `Array(array(expr))
  | `Object(array((objectKey, expr)))
  | `Function(function_(option(ident), block))
  | `ArrowFunction(
      function_(unit, [ | `Block(array(statement)) | `Return(expr)]),
    )
  | `Class(option(ident), option(expr), array(classProperty))
  | `Dot(expr, ident)
  | `Call(expr, array(expr))
  | `ArrayGet(expr, expr)
  | `Binary(string, expr, expr)
  | `Unary(string, expr)
  | `Ternary(expr, expr, expr)
  | `New(expr)
  | `Jsx(jsxNode)
  | `Await(expr)
];
