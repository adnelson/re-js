// Tools for rendering an AST as a JavaScript string

open Ast;

type rawJS = string;

let map: 'a 'b. (array('a), 'a => 'b) => array('b) = Belt.Array.map;
let join: array(rawJS) => rawJS = Js.Array.joinWith("");
let spaces: array(rawJS) => rawJS = Js.Array.joinWith(" ");
let commas: array(rawJS) => rawJS = Js.Array.joinWith(",");
let semis: array(rawJS) => rawJS = Js.Array.joinWith(";");
let parens: rawJS => rawJS = s => "(" ++ s ++ ")";
let brackets: rawJS => rawJS = s => "[" ++ s ++ "]";
let curlies: rawJS => rawJS = s => "{" ++ s ++ "}";

let rec block: block => rawJS =
  (`Block(statements)) => statements->map(statement)->semis->curlies

and ident: ident => rawJS = Identifier.toString

and varDecKind =
  fun
  | `Var => "var"
  | `Let => "let"
  | `Const => "const"

and varDec: varDec => rawJS =
  ({kind, varName}) => kind->varDecKind ++ " " ++ varName->ident

and destructureObject: destructureObject => rawJS =
  dj =>
    switch (dj) {
    | `Name(i, None)
    | `NameWithInner(i, [||]) => i->ident
    | `Name(i, Some(alias)) => [|i->ident, ": ", alias->ident|]->join
    | `NameWithInner(i, inner) =>
      [|i->ident, ": ", inner->map(destructureObject)->commas->curlies|]
      ->join
    | `MultipleDestructures(dos) =>
      dos->map(destructureObject)->commas->curlies
    }

and importable: importable => rawJS =
  i =>
    switch (i) {
    | `StarAs(i) => "* as " ++ i->ident
    | `Destructure(dobj) => dobj->destructureObject
    }

and import: import => rawJS =
  ({what, from}) =>
    [|
      "import",
      what->Belt.Array.map(importable)->commas,
      "from",
      Js.Json.(from->string->stringify),
    |]
    ->spaces

and topLevelStatement: topLevelStatement => rawJS =
  tls =>
    switch (tls) {
    | `Statement(s) => s->statement
    | `Export(d) => [|"export", d->declaration|]->spaces
    }

and defaultExport: defaultExport => rawJS =
  ex =>
    [|
      "export default",
      switch (ex) {
      | `Declaration(d) => d->declaration
      | `ExportExpr(e) => e->expr
      },
    |]
    ->spaces

and module_ = ({imports, statements, defaultExport: de}) =>
  Belt.Array.(
    imports
    ->map(import)
    ->concat(statements->map(topLevelStatement))
    ->concat(de->Belt.Option.mapWithDefault([||], d => [|d->defaultExport|]))
  )
  ->semis

and assignable: assignable => rawJS =
  a =>
    switch (a) {
    | `AssignVar(v) => v->ident
    | `AssignObjectDot(`Variable(v), key) => v->ident ++ "." ++ key->ident
    | `AssignObjectDot(e, key) => e->expr->parens ++ "." ++ key->ident
    | `AssignObjectBrackets(`Variable(v), e) => v->ident ++ e->expr->brackets
    | `AssignObjectBrackets(e1, e2) => e1->expr->parens ++ e2->expr->brackets
    }

and staticness: staticness => rawJS =
  s =>
    switch (s) {
    | `Static => "static"
    | `NotStatic => ""
    }

and class_: (option(ident), option(expr), array(classProperty)) => rawJS =
  (className, extends, properties) =>
    [|
      [|
        "class",
        className->Belt.Option.mapWithDefault("", ident),
        extends->Belt.Option.mapWithDefault("", e =>
          "extends " ++ e->expr->parens
        ),
      |]
      ->spaces,
      properties
      ->Belt.Array.map(p =>
          switch (p) {
          | `Function(static, f) =>
            [|
              static->staticness,
              f.sync->synchronicity,
              f.name->ident,
              f.params->map(ident)->commas->parens,
              f.body->block,
            |]
            ->spaces
          | `ClassVariable(name, e) => [|name->ident, "=", e->expr|]->spaces
          }
        )
      ->spaces
      ->curlies,
    |]
    ->join

and declaration: declaration => rawJS =
  d =>
    switch (d) {
    | `DeclareVar(vd, None) => vd->varDec
    | `DeclareVar(vd, Some(e)) => [|vd->varDec, "=", e->expr|]->spaces
    | `DeclareFunction(f) => `Function({...f, name: Some(f.name)})->expr
    | `DeclareClass(name, extends, properties) =>
      `Class((Some(name), extends, properties))->expr
    }

and statement: statement => rawJS =
  s =>
    switch (s) {
    | `Expr(e) => e->expr
    | `Declaration(d) => d->declaration
    // `if` without `else`
    | `If(e, b, None) => [|"if", e->expr->parens, b->block|]->join
    // `if {} else if {}`
    | `If(e, b1, Some(`Block([|`If(_) as i|]))) =>
      [|`If((e, b1, None))->statement, "else", i->statement|]->join
    // `if {} else {}`
    | `If(e, b1, Some(b2)) =>
      [|`If((e, b1, None))->statement, "else", b2->block|]->join
    | `While(e, b) => "while" ++ e->expr->parens ++ b->block
    | `For(v, e2, e3, b) =>
      [|"for", [|v->varDec, e2->expr, e3->expr|]->semis->parens, b->block|]
      ->join
    | `Assign(a, e) => a->assignable ++ " = " ++ e->expr
    | `Return(None) => "return"
    | `Return(Some(e)) => "return " ++ e->expr
    | `Break => "break"
    | `Throw(e) => "throw " ++ e->expr
    }

and jsxElement: jsxElement => rawJS =
  ({elementName: name, elementProps: props, elementChildren: children}) =>
    [|
      "<" ++ name->ident,
      switch (props) {
      | [||] => ">"
      | _ =>
        " "
        ++ props
           ->Belt.Array.map(((k, v)) => k->ident ++ "=" ++ v->expr->curlies)
           ->join
        ++ ">"
      },
      children->Belt.Array.map(jsxNode)->join,
      "</" ++ name->ident ++ ">",
    |]
    ->join

and jsxNode: jsxNode => rawJS =
  j =>
    switch (j) {
    | `String(s) => s // TODO escape in some way?
    | `Expr(e) => e->expr->curlies
    | `Fragment(children) =>
      [|"<>", children->Belt.Array.map(jsxNode)->join, "</>"|]->join
    | `Element(element) => element->jsxElement
    }

and synchronicity = s =>
  switch (s) {
  | `Sync => ""
  | `Async => "async"
  }

and objectKey = ok =>
  switch (ok) {
  | `I(i) => i->ident
  | `S(s) => s->Json.Encode.string->Js.Json.stringify
  | `E(e) => e->expr->brackets
  }

and expr: expr => rawJS =
  e =>
    switch (e) {
    | `This => "this"
    | `Null => "null"
    | `Undefined => "undefined"
    | `Bool(b) => b ? "true" : "false"
    | `Number(n) => n->Js.Float.toString
    | `Variable(i) => i->ident
    | `String(s) => s->Json.Encode.string->Js.Json.stringify
    | `InterpolatedString(parts) =>
      "`"
      ++ parts
         ->Belt.Array.map(p =>
             switch (p) {
             | `S(s) => s |> Js.String.replace("`", "\\`")
             | `E(e) => "$" ++ e->expr->curlies
             }
           )
         ->join
      ++ "`"
    | `Json(json) => json->Js.Json.stringify
    | `Class(name, extends, properties) => class_(name, extends, properties)
    | `ArrowFunction(f) =>
      [|
        f.sync->synchronicity,
        f.params->map(ident)->commas->parens,
        "=>",
        switch (f.body) {
        | `Return(e) => e->expr->parens
        | `Block(body) => `Block(body)->block
        },
      |]
      ->spaces
    | `Function(f) =>
      [|
        f.sync->synchronicity,
        f.name->Belt.Option.mapWithDefault("", ident),
        f.params->map(ident)->commas->parens,
        f.body->block,
      |]
      ->join
    | `Array(exprs) => exprs->map(expr)->commas->brackets
    | `Object(kvs) =>
      kvs->map(((k, e)) => k->objectKey ++ ": " ++ e->expr)->commas->curlies
    | `Dot(e, key) => e->expr ++ "." ++ key->ident
    | `Call(e, es) => e->expr ++ es->map(expr)->commas->parens
    | `Binary(op, e1, e2) => [|e1->expr->parens, op, e2->expr->parens|]->join
    | `Unary(op, e) => op ++ e->expr->parens
    | `Ternary(e1, e2, e3) =>
      [|e1->expr, " ? ", e2->expr, " : ", e3->expr|]->join->parens
    | `ArrayGet(e1, e2) => e1->expr ++ e2->expr->brackets
    | `New(e) => "new " ++ e->expr
    | `Jsx(node) => node->jsxNode
    | `Await(e) => "await " ++ e->expr
    };
