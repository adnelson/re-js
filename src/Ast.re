type ident = Identifier.t

and varDecKind = [ | `Var | `Let | `Const]

and varDec = {
  kind: varDecKind,
  pattern,
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

// A pattern which can be used to destructure an object in an assignment
// or function argument.
//
// Examples (omitting `None` if no inner pattern):
//
// foo
//  ==> `Name("foo")
// [ foo, bar ]
//  ==> `DestructureArray([|`Name("foo"), `Name("bar")|])
// { foo, bar }
//  ==> `DestructureObject([|`Name("foo"), `Name("bar") |])
// { foo, bar: { baz } }
// ==> `DestructureObject([|`Name("foo"), `Name("bar", Some(name("baz")))|])
//
and pattern = [
  | `Name(ident, option(destructurePattern))
  | `DestructureArray(array(pattern))
  | `DestructureObject(array(pattern))
]

// Define this as a separate type because Name patterns can't have other
// Names as inner patterns
and destructurePattern = [
  | `DestructureArray(array(pattern))
  | `DestructureObject(array(pattern))
]

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
  params: array(pattern),
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
