open Ast;

let int: int => expr = i => `Number(i->float_of_int);
let bool: bool => expr = b => `Bool(b);
let float: float => expr = f => `Number(f);
let string: string => expr = s => `String(s);
let array: array(expr) => expr = exprs => `Array(exprs);
let null: expr = `Null;
let plus = (e1, e2) => `Binary(("+", e1, e2));
let minus = (e1, e2) => `Binary(("-", e1, e2));
let times = (e1, e2) => `Binary(("*", e1, e2));
let divide = (e1, e2) => `Binary(("/", e1, e2));

let ident = Identifier.fromString;

let idents = is => is->Belt.Array.map(ident);

let var = name => `Variable(name->ident);

let dots = (e, keyChain): expr =>
  keyChain->Belt.Array.reduce(e, (e, i) => `Dot((e, i->ident)));

let call0 = f => `Call((f, [||]));
let call1 = (f, x) => `Call((f, [|x|]));
let call2 = (f, x, y) => `Call((f, [|x, y|]));

let consoleDotLog = args => `Call((dots(var("console"), [|"log"|]), args));
let consoleDotLog1 = e => consoleDotLog([|e|]);
let consoleDotLog2 = (e1, e2) => consoleDotLog([|e1, e2|]);

let declareVar = (~kind=`Const, i, e): declaration =>
  `DeclareVar(({kind, varName: i->ident}, Some(e)));

let exportDeclareVar = (~kind=`Const, i, e): topLevelStatement =>
  `Export(`DeclareVar(({kind, varName: i->ident}, e)));

let block1: statement => block = s => `Block([|s|]);

let functionProperty =
    (~static=false, ~sync=`Sync, name, params, body): classProperty =>
  `Function((
    static ? `Static : `NotStatic,
    {sync, name: ident(name), params: params->idents, body},
  ));

let classVariable = (name, expr): classProperty =>
  `ClassVariable((name->ident, expr));

let func = (~sync=`Sync, ~name=?, params, body): expr =>
  `Function({
    sync,
    name: name->Belt.Option.map(ident),
    params: params->Belt.Array.map(ident),
    body: `Block(body),
  });
let arrowFunc = (~sync=`Sync, params, body): expr =>
  `ArrowFunction({
    sync,
    name: (),
    params: params->idents,
    body: `Block(body),
  });
let arrowFunc1 = (~sync=`Sync, params, e): expr =>
  `ArrowFunction({sync, name: (), params: params->idents, body: `Return(e)});

let class_ = (~name=?, ~extends: option(expr)=?, properties): expr =>
  `Class((name->Belt.Option.map(ident), extends, properties));

let jsxElement = (~props=?, ~children=?, name) => {
  elementName: name->ident,
  elementProps:
    props->Belt.Option.mapWithDefault([||], a =>
      a->Belt.Array.map(((i, e)) => (i->ident, e))
    ),
  elementChildren: children->Belt.Option.getWithDefault([||]),
};

let addProp = (e, key, prop) => {
  ...e,
  elementProps: e.elementProps->Belt.Array.concat([|(key->ident, prop)|]),
};

let object_ = (kvs: array((ident, expr))): expr =>
  `Object(kvs->Belt.Array.map(((k, v)) => (`I(k), v)));

let object0 = object_([||]);

let object1 = (k, v) => object_([|(k->ident, v)|]);
let object2 = ((k1, v1), (k2, v2)) =>
  object_([|(k1->ident, v1), (k2->ident, v2)|]);
let object3 = ((k1, v1), (k2, v2), (k3, v3)) =>
  object_([|(k1->ident, v1), (k2->ident, v2), (k3->ident, v3)|]);
let object4 = ((k1, v1), (k2, v2), (k3, v3), (k4, v4)) =>
  object_([|
    (k1->ident, v1),
    (k2->ident, v2),
    (k3->ident, v3),
    (k4->ident, v4),
  |]);

let importDefault = (~name, ~from) => {
  from,
  what: [|`Destructure(`Name((name->ident, None)))|],
};

let importNames = (~from, names) => {
  from,
  what: [|
    `Destructure(
      `MultipleDestructures(
        names->Belt.Array.map(n => `Name((n->ident, None))),
      ),
    ),
  |],
};

// Create an AST for a react component class.
let reactComponentClass = (~name, ~pure, renderBody) => {
  let extends = dots("React"->var, [|(pure ? "Pure" : "") ++ "Component"|]);
  class_(~name, ~extends, [|functionProperty("render", [||], renderBody)|]);
};

let rec json: Js.Json.t => expr =
  j =>
    switch (j->Js.Json.classify) {
    | JSONString(s) => s->string
    | JSONNumber(n) => n->float
    | JSONTrue => true->bool
    | JSONFalse => false->bool
    | JSONNull => null
    | JSONArray(arr) => arr->Belt.Array.map(json)->array
    | JSONObject(obj) =>
      `Object(
        obj
        ->Js.Dict.entries
        ->Utils.Array.map(((k, v)) => (`S(k), v->json)),
      )
    };
